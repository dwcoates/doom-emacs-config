# Deploy mediator: plan (owner request, 2026-11-13)

## What the owner asked for

- A hidden, daemon-owned session per repository ("deploy-mediator") that runs
  after EVERY deploy and before the merge queue may advance.
- It checks the deploy window for errors and warnings in every system's logs,
  `--doctor`, and latency regressions in the metrics store.
- Then exactly one of:
  - approve: the queue proceeds; the mediator relinquishes until the next deploy;
  - escalate: the fix is too opinionated (a reasonable user-facing UX
    alternative exists) or not something it can confidently do: it tells the
    user and the queue proceeds;
  - remediate: it implements the fixes itself, runs the full suite as the
    queue does (governed, reuse on), lands directly on the main branch
    (`land-direct.sh`), deploys, and the process starts over on that deploy.
- It never creates children or suggests; it remediates itself.
- Its context is compacted at the start of every round.
- No round limit: endless remediation is an implication of the design, not
  logic in it.
- Model opus, effort medium.
- Lenient on what "only the user can fix" means to start with; the owner adds
  restrictions as they see it act.
- Shown specially in the sidebar's merge queue section.

## Design

1. CONFIG. `repoconfig.v1.LifecycleConfig.deploy_mediator`
   (`DeployMediator { name }`), tag 11. The doom repository sets
   `deploy_mediator: {name: deploy-mediator}`. A repository without it has no
   mediator and nothing below applies.
2. DEBT, DURABLE. `merge_queue_repos` gains `mediation_owed_at` (a deploy's
   start, ms; NULL when none is owed), `mediation_prompted_at` and
   `mediation_version`. EVERY deploy of the daemon's build (a queue landing's
   rollout and `RollOutBuild` alike) owes a mediation for each repository
   with a mediator, in one write at the deploy's start, so a handover between
   the deploy and the check leaves it owed.
3. HOLD. `merge.queueHeld` (the one advance gate) is also true while a
   mediation is owed. Nothing else gates on it.
4. DRIVER (serving daemon only, `ServesQueue`). When a mediation is owed and
   not yet prompted for this deploy (`prompted_at < owed_at`), the daemon is
   up, and the mediator is free: ensure the mediator workspace exists (created
   hidden, opus/medium, on the repository's main branch tip), compact it, and
   prompt it with the mediation brief (the window's start, the version, how to
   decide). Record `prompted_at`.
5. DECISION. `agent-repld merge-queue mediation approve|escalate -repository
   here` from the mediator workspace (rpc `UpdateMergeQueueMediation`). Both
   clear the debt and kick the queue; escalate also notifies the user.
   Remediation needs no verb: the mediator's own deploy owes the next round.
6. A TURN THAT DECIDES NOTHING. At the mediator's turn end, a debt still owed
   for the deploy it was prompted on (no verb, no new deploy) is released
   loudly: ERROR record, desktop notification, queue kicked.
7. SESSION. The mediator's session landing is a new arm
   (`shim.v1.SessionProfileAgentLanding.mediating`), whose metaprompt section
   is `metaprompt-deploy-mediator.md`.
8. SIDEBAR. The roster's merge queue carries the mediation
   (`frontend.v1.RosterMergeQueue.mediation`: the mediator's name, and
   checking / remediating / none), drawn as a pinned row at the queue's head.

## Progress

- [x] 1 config (schema and check; the `deploy_mediator` line in
  `.agent-repl/lifecycle.yaml` lands only AFTER the daemon that parses it is
  deployed: the running daemon reads the worktree's and master's config and
  refuses an unknown field, so landing them together breaks the deploy)
- [x] 2 debt (wsm layout 132, internal/wsm/mediation.go)
- [x] 3 hold (merge.queueHeld)
- [ ] 4 driver
- [ ] 5 decision rpc + CLI
- [ ] 6 undecided turn end
- [ ] 7 session landing + metaprompt section
- [ ] 8 sidebar
- [ ] full suite, land, deploy
