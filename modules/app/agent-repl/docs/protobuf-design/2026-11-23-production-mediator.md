# Production mediator: contract record

The production mediator is a sibling of the deploy mediator. After each deploy it reviews what production logged while the previous version served on its own, and hands what it finds to one-shot children that land at the front of the merge queue. The plan is `docs/investigations/2026-11-23-production-mediator-plan.md`. This record covers the contract the plan needs. It was designed autonomously: the owner gave the requirements and rulings, and asked for no review of the contract's shape.

## Assumptions

- **Objective.** Review production between landings and dispatch fixes for new warnings, errors and regressions, without ever blocking a deploy.
- **Frontend.** None. The production mediator is a hidden workspace like the deploy mediator, and its children are ordinary one-shot children. Nothing new is drawn, so no `frontend.v1` message changes.
- **Systems.**
  - The daemon serves the new rpc and reads the new config.
  - The shim gains one landing arm and one metaprompt.
  - The sidecar and the webapp forward log records, so they adopt the typed site key.
  - `agent-repld` gains the `production-mediation` verbs.
- **Non-additive changes.**
  - `agentrepl.v1.ClientLogRecord.site` and `agentrepl.v1.LogSite.key` were bare strings. Both become `agentrepl.v1.LogSiteKey`, and their old tags are reserved.
  - `agentrepl.v1.FinishWorkspaceRequest.placement` becomes `optional`.

## Iteration sequence

Autonomous mode uses the default order: concerns first, top-down by containment.

1. Configuration (`repoconfig.v1`).
2. The session's identity (`shim.v1` landing, `conversation.v1` prompt origin).
3. The shared site identity (`agentrepl.v1.LogSiteKey`).
4. The one-shot's stored finish placement (create and finish).
5. The new rpc, `agentrepl.v1.UpdateProductionMediation`.

## Landed changes

### 1. Configuration: `ProductionChecks` and `MediatorAgent` (2026-11-23)

- **What.**
  - `repoconfig.v1.LifecycleConfig.production_checks` (tag 14) is new. Its presence turns the checks on, as `deploy_checks` does.
  - `repoconfig.v1.MediatorAgent` holds an optional model string and an optional `conversation.v1.AgentEffortLevel`.
  - Both `repoconfig.v1.DeployChecks.agent` and `repoconfig.v1.ProductionChecks.agent` hold a `repoconfig.v1.MediatorAgent`.
- **Why.** The owner asked for both mediators' models to be configurable, with this repository setting both to `fable`, and ruled that effort is configurable beside the model.
- **Why the model is a string.**
  - `conversation.v1.AgentModel` is a typed echo token that the daemon mints in served options, and a hand-written YAML file cannot echo a served token.
  - The string matches `agentrepl.v1.CreateWorkspaceRequest.model`, which is how a person names a model elsewhere.
- **Children are not configured.** The owner ruled that children run on `opus` by default and never follow the mediator's model. So `repoconfig.v1.ProductionChecks` has no field for them.
- **Consequences.**
  - `repoconfig.v1` now imports `conversation/v1/api.proto`. `conversation.v1` is the leaf every package may import.
  - The YAML spells effort as the enum value's name (`AGENT_EFFORT_LEVEL_LOW`), because the daemon decodes the file with protojson.
  - A model the login does not offer fails the mediator's session start loudly. The daemon never falls back to another model.

### 2. The production mediator's session identity (2026-11-23)

- **What.**
  - `shim.v1.SessionProfileAgentLanding.production_mediator` (tag 6) holds the new empty message `shim.v1.SessionProfileAgentLandingProductionMediator`.
  - `conversation.v1.PromptOrigin.PROMPT_ORIGIN_PRODUCTION_MEDIATION` (55, after master took 54 for `PROMPT_ORIGIN_SLACK_THREAD_NOTICE`) is new, with spend cause `AUTOMATED`.
- **Why.** The shim picks the metaprompt from the landing arm. The owner ruled that the production mediator's metaprompt is its own text, so it needs its own arm.
- **Why the spend cause is `AUTOMATED`.** The deploy mediation's cause is `MERGE` because its check holds the queue. The production check holds nothing.

### 3. `agentrepl.v1.LogSiteKey` (2026-11-23)

- **What.**
  - `agentrepl/v1/log_site_key.proto` declares `agentrepl.v1.LogSiteKey { string hex = 1; }`.
  - `agentrepl.v1.ClientLogRecord.site_key` (tag 14) replaces `site` (tag 12, now reserved).
  - `agentrepl.v1.LogSite.site` (tag 10) replaces `key` (tag 1, now reserved).
- **Why.**
  - The mediator names sites back to the daemon in its findings, and the daemon records which child owns each one. That makes the key a join key.
  - Under the typed-identity rule, a join key is one message that every user of it imports. It is never a bare string respelled at each site.
- **Why the tags are reserved instead of reused.** The sidecar is a separate launchd service, so it can run a build older than the daemon's. A string read as a message at the same tag would fail to decode instead of being refused cleanly.
- **Consequences (implementation).**
  - Daemon:
    - `daemon/internal/server/admin.go` `clientRecord` reads `GetSiteKey().GetHex()`.
    - `daemon/internal/server/validate.go` validates `record.site_key`, and the refusal's field path reads `record.site_key`.
    - `daemon/internal/server/listlogsites.go` serves `Site: &LogSiteKey{Hex: …}`.
  - Sidecar: `agent-shim/claude/shim-sidecar/internal/daemonclient/client.go` forwards `SiteKey`.
  - Webapp: `webapp/src/log.ts` `buildClientLogRecord` sets `siteKey: { hex }`.
  - The `agent-repld logs sites` JSON output now prints `site: {hex}` instead of `key`.
  - Tests that built records by the old field were adapted mechanically. The adaptation was a pure rename of the field, and no expected behavior changed.

### 4. The stored finish placement (2026-11-23)

- **What.**
  - `agentrepl.v1.CreateWorkspaceOneShot.finish_placement` (tag 9) is new, an optional `agentrepl.v1.MergeQueuePlacement`.
  - `agentrepl.v1.FinishWorkspaceRequest.placement` becomes `optional`.
  - `agentrepl.v1.FinishWorkspaceError.placement_conflicts_with_stored` (tag 19) is new and carries both placements.
- **Why.** The owner ruled that a child's finish strategy is recorded when it is created, and that a finish naming a conflicting placement is refused. The mediator decides where its children land in line, not the children.
- **Closed question: why this is not a flag on the production rpc alone.** The owner asked for a `-finish front` flag on create. A field on every one-shot serves both the CLI and the mediator's dispatch, which creates its children through the same one-shot path.
- **Consequence.** A finish with no placement remains a validation failure, except when the one-shot stored a placement.

### 5. `agentrepl.v1.UpdateProductionMediation` (2026-11-23)

- **What.** A new rpc in the agent API section, declared in `endpoint_update_production_mediation.proto`.
  - The request names the mediator, plus an action: `decide` or `fold`.
  - `decide` holds a summary and either `nothing_to_do` or `dispatch`.
  - `dispatch` holds one or more `agentrepl.v1.ProductionChild`.
    - Each child has an optional name and a work arm: `remediation` or `investigation`.
    - Each child has one or more `agentrepl.v1.ProductionFinding`, each with a statement, the site keys it covers, and verbatim evidence.
  - `fold` names the child to fold away and the child that survives.
  - The success arms mirror the actions. `dispatched` returns the created children in request order.
  - Refusals:
    - `unknown_workspace`, `workspace_ref_mismatch` and `transferring_away`.
    - `not_production_mediator` and `no_check_owed`.
    - `site_owned`, held by an open child or repeated within the dispatch.
    - `not_foldable`.
    - `child_refused`, which undoes the whole dispatch.
- **Why.**
  - The owner ruled that every child is created in one daemon call, that each child is told its siblings, and that related findings go to one child.
  - The owner ruled that the mediator may fold one child into another when their work converges.
  - The owner ruled that the daemon records site ownership, so no site is worked twice.
- **Closed question: why not reuse `agentrepl.v1.UpdateMergeQueue.mediation`.** That arm is a decision on a queue hold. The production check holds no queue, and its decision creates workspaces. Reusing it would put approve and escalate beside a dispatch that means something different, and would route a production decision through the queue's refusals.
- **Why a finding's sites may be empty.** A performance regression read from the metrics store has no log site. So evidence is required and sites are not.
- **Consequences.**
  - `agentrepl.v1.CreateWorkspaceError.requested_by_production_mediator` (tag 64) refuses a direct create by the production mediator, so every child it makes goes through a dispatch and is recorded with its sites.
  - The daemon creates every child of a dispatch before starting any. A refusal undoes the ones already made, so a dispatch is all or nothing.
  - `daemon/internal/server/productionmediation.go` answers Unimplemented until the daemon side is built on this branch. The branch does not land until that is replaced.

## Obviated-declaration sweep

- `agentrepl.v1.UpdateMergeQueueMediation` and its arms are still the deploy mediator's, so nothing became obviated.
- `CreateWorkspaceRequestedByDeployMediator` still refuses the deploy mediator.
- The sweep found nothing to remove.
