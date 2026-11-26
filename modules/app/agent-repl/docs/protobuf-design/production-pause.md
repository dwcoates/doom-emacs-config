# Production mediation pause

The owner wants a pause for the production mediator (rulings of 2026-11-25,
relayed through `production-mediator-planning`). The pause belongs to the
daemon and is kept per repository, so it survives restarts. Pausing interrupts
the mediator's current turn at once, and its next prompt says production
mediation is paused, so it dispatches nothing and does not work around the
pause. While paused the daemon tells the mediator no spans. Spans that end
during a pause are never reviewed, and checks start again at the first landing
after the resume. Agents pause and resume through `agent-repld
production-mediation pause|resume -repository here|DIR`, and every workspace's
topbar shows a chip right of the persistent-wifi chip, only in repositories
whose production checks are on, which shows the standing and switches it on a
click.

## Assumptions (autonomous design)

- Objective: a durable, per-repository switch that stops the production
  mediator and its checks, reachable from the command line and the topbar.
- Frontend: yes, one topbar chip. figma→idl governs it.
- Systems: the protobuf contract, the daemon (store, merge orchestrator,
  server, prompt handler, topbar resolver, workspace verbs, command line), the
  webapp topbar, and the production mediator's harness (its metaprompt and its
  intake briefs).
- Non-additive changes: none to the wire. The daemon's
  `wsm.DB.OweProductionCheck` changes its answer from a boolean to an outcome
  that names a paused span.

## Iteration sequence

1. Transport: ConnectRPC, the daemon's one agent API service.
2. Endpoint inventory: one new rpc, `UpdateProductionPause`, in the runtime
   section beside `UpdateDeployHold`.
3. Conventions: the standing `oneof result { success; error; }`.
4. Shapes: `UpdateProductionPause`, then the `paused` refusal of
   `UpdateProductionMediation`, then the topbar chip.

## Landed changes

### `agentrepl.v1.UpdateProductionPause`

- What: a new endpoint file with a repository-scoped request whose arm is the
  action (`pause`, `resume`). Success says what was done; `paused` names the
  mediator whose turn it interrupted (unset when none ran) and how many spans
  it dropped. Errors: `already_paused`, `not_paused`, `unknown_repository`,
  `checks_off`.
- Why: the pause is the repository's, not the mediator's, so the request
  names a repository, as `UpdateDeployHold` does; the mediator-only
  `UpdateProductionMediation` would refuse every caller but the mediator.
- Why a separate rpc and not arms on `UpdateProductionMediation`: that rpc's
  caller is the mediator itself and its refusals are about the mediator's
  standing; a pause is asked by the owner, by any agent, and by the topbar,
  none of them the mediator.
- Consequences: dropping undecided spans at the pause is what makes "spans
  that end during a pause are never reviewed" hold for a span already owed
  when the pause began. A span the mediator was being told of is dropped too,
  and its brief is retired from the prompt queue when still held.
- Consequence: "checks start again at the first landing after resume" is read
  as the first landing after the resume opening a fresh span. The span that
  landing closes began during the pause, so it is skipped, and the first span
  told runs from that landing to the next.

### `agentrepl.v1.UpdateProductionMediationError.paused`

- What: a new refusal arm, empty, for a decision or a fold asked while the
  checks are paused.
- Why: the mediator must not work around the pause; refusing with a named arm
  tells it why, where `no_check_owed` would read as an ordinary empty queue.
- Folds are refused as well as decisions: while paused the mediator takes no
  production action at all, which is the plainer rule for its harness.

### `frontend.v1.TopbarProductionChecks`

- What: an optional chip on `frontend.v1.TopbarView` (tag 18), set only for a
  repository whose lifecycle config turns production checks on. Its standing
  arm is `running` or `paused`; it carries the repository a click acts on
  (`workspace.v1.RepositoryRef`, imported, echoed in the request) and a
  daemon-composed tooltip naming the mediator.
- Layout: the left status run reads
  `[queue bypass] [remote control] [persistent wifi] [production checks]`.
  The new chip is one box with one glyph on a disc, as the wifi chip is.
- Why the repository rides the chip: the click must name what it acts on, and
  the client derives nothing; the topbar's context knows the workspace, not
  its repository.
- Consequence: the standing is restated to every open workspace of the
  repository on each pause or resume. A lifecycle config edit that turns the
  checks on or off reaches a workspace's chip at its next naming (daemon
  restart or workspace registration), not at once.

## Sweep

Nothing became obviated: every message added is new and every existing one
keeps its consumers.

## Implementation notes

- The store owns the pause (`daemon/internal/wsm/productionpause.go`, layout
  217). No undecided check exists while a repository is paused: the pause
  deletes them in the write that takes it, and no span is owed while it
  stands, so every mark, resumption or decision of a span during a pause finds
  nothing to touch.
- The merge orchestrator marks and submits every span brief under the
  repository's production lock, and the pause takes the same lock to drop the
  spans and retire a brief still held in the mediator's prompt queue. It
  interrupts the mediator's running turn after.
- Accepted edge: a brief the prompt queue is delivering at the very moment of
  the pause is neither retirable nor yet a running turn, so it may start a
  turn after the interrupt. That turn's note-free brief cannot be acted on:
  every decision and fold is refused with `paused`.
- The note a paused mediator's prompts open with is read from the standing
  pause on every prompt that carries workspace notes, never owed, so it can
  neither outlive the pause nor miss a prompt. Daemon-born briefs carry no
  workspace note, and none is sent during a pause.
- Spans a pause leaves unreviewed are counted under
  `agentrepl_production_checks_total{outcome="paused"}`.
