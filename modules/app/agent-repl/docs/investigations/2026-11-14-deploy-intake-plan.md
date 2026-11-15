# Deploy intake and remediation: plan (owner request, 2026-11-14)

Supersedes the standing deploy mediator
(`docs/investigations/2026-11-13-deploy-mediator-plan.md`) once built. It is
written for review: nothing below is built yet.

## What the owner asked for

- A DEPLOY INTAKE: a hidden workspace that assesses each deploy, decides
  whether what it finds warrants agentic work, and when it does, dispatches
  it.
- A DEPLOY REMEDIATION workspace: visible in the sidebar like any other
  workspace, doing the fix, whose finish is handled specially:
  - it runs the gate, and on a failure is resumed to fix its tests and finish
    again, as a one-shot is;
  - it lands directly on the main branch, never through the queue;
  - it deploys, and the deploy is assessed in turn.
- The intake persists across the attempts of one problem, so it can judge
  when to escalate instead of trying again.
- The intake watches the remediation's deploy too, so a deploy that broke the
  deploy harness itself is caught and dispatched the same way.
- A real pause is never overridden.
- In-flight queue work is never interrupted, and wasted work is avoided where
  it costs no latency.
- The purple label beside the repository's name selects the workspace at
  work.
- The work is measured: how often a check finds nothing, dispatches,
  escalates; how long each takes; how many remediations run in a row.

## What the standing mediator taught (2026-11-14)

- It works: it read windows, found real defects, fixed them with tests,
  landed and deployed. But from 14:36 to past 16:30 it deployed four fixes in
  a row and approved none.
- Its brief makes EVERY warning in the window its job, and its rules forbid
  calling one pre-existing. So defects the deploy did not cause (the
  worktree reaper, the shim tails) held the queue until fixed.
- Its own deploys feed the next window: three of its five landings fixed the
  mediation machinery its previous deploy exposed.
- Its deploy cut its own round (16:10, "the watcher closed while a wait was
  standing"): a step that spans the deploy it causes must be durable.
- One standing session, compacted every round, carries no useful memory of
  earlier attempts, and is hidden, so its work cannot be watched.

## Design

### Roles

1. THE CHAIN. A chain begins at a deploy whose mediation nothing else was
   owed beside, and ends when the queue advances past it. It is already
   durable (wsm layout 135: `Mediation.ChainStartedAt`, `ChainRedeploys`).
2. THE INTAKE, one per chain. Created hidden, with a fresh context, when the
   chain's first deploy has rolled out; told of every deploy of its chain in
   turn, so every attempt is in its context; retired when the chain ends. Its
   brief carries the history the store keeps: the chain so far and how
   recent chains ended. It never edits code.
3. THE REMEDIATION, at most one live per chain. Created visible by the
   intake's first dispatch, a child of nothing, with a fresh context and the
   intake's account of the defect as its prompt. It CLOSES at its finish's
   deploy like any finished workspace, so no live session spans the daemon
   restart. A later dispatch in the same chain (owner ruling, 2026-11-14):
   - RESTORES the chain's last remediation workspace when the defect is in,
     or caused by, its own fix: it knows what it changed, why, and what it
     ruled out. It reopens on the serving daemon from its stored
     conversation; its worktree, removed by its landing, is recreated on its
     branch at the main branch's tip, which holds its landed fix. The
     intake's brief to it states only the new findings, and it compacts
     first when its context is large.
   - CREATES a fresh remediation workspace when the defect is unrelated to
     the earlier fix, so it inherits no irrelevant context or approach.
   - The intake states which, and why, in its dispatch. The bound on
     remediations in a row (4, ruling 2) is what stops a restored
     workspace whose approach is the problem from trying again.

### The intake's decision

Each deploy's check ends in exactly one of:

- `approve`: nothing this deploy caused is wrong. The hold is released and
  the queue advances.
- `escalate`: something is wrong that should go before the user (no
  confident fix, a user-facing choice, or a NEW test reuse fact). The hold
  is released and the chain ends. The intake never talks to the user (owner
  ruling, 2026-11-15): the daemon makes a NEW VISIBLE workspace, on opus at
  medium effort, whose prompt is the summary and the intake's findings with
  their evidence, and a desktop notification whose click opens it. Every
  escalation the daemon makes for the intake (a turn that decided nothing,
  a remediation given up past its fixing bound or closed before it landed)
  goes the same way.
- `dispatch`: a defect THIS DEPLOY CAUSED, which an agent can fix. The hold
  stays, owned by the remediation workspace (below).
- Every defect the deploy did NOT cause (it predates the window's deploy, or
  it is noise of the rollout the harness already owns) does not hold the
  queue: the intake files it as a one-shot through the queue, or as a todo
  row, and decides the deploy on what remains (ruling 1).

A chain whose remediation count reaches the bound (4, ruling 2) is
escalated by the intake's own rule, stated in its brief from the chain's
count.

### The remediation's finish

`workspace finished` in a deploy remediation workspace runs a finish of its
own, each step recorded durably before it starts, so the daemon that serves
after the deploy picks it up where it stood:

1. The gate: the full suite, as the queue's gate runs it. A failure resumes
   the remediation's session with the failures, one-shot style, to fix them
   and finish again, up to the queue's fixing bound; past it, the intake is
   told and escalates.
2. Land directly on the main branch: the same merge, changelog line and
   version tag `land-direct.sh` makes, owned by the daemon.
3. Deploy.
4. The deploy owes the chain's intake a check (the hold stays). The finish
   NEVER releases the hold and NEVER resumes the queue: only the intake's
   approval of the fix's deploy does.

The remediation workspace closes at its finish's deploy (step 3), and is
restored by a later dispatch of its chain as above.

### The hold, the pause, and the queue's work

- The hold is the existing mediation hold (`merge.queueHeld`), not a pause:
  it is drawn as the purple label, never as paused, and a pause set by the
  user or an agent is never touched by anything here.
- The hold's owner is always recorded: the intake while it assesses, the
  remediation while it is open. A remediation workspace that closes without
  landing hands the hold back to the intake, which escalates. Nothing leaves
  the queue held with no owner.
- Merges already working in line are never interrupted. While the intake
  assesses, they also start new work as usual: most checks approve, and
  stopping them would add the assessment to every merge's wait. Once the
  intake dispatches, they start no NEW rebase, conflict resolution or gate
  until the fix lands, since its landing would make that work stale;
  whatever is running finishes its step.
- TODAY (verified 2026-11-14): the mediation hold is one input of
  `merge.queueHeld`, the same gate a pause and a handover are. It suspends
  no running merge (only a pause and a handover do, `suspendHeld`), but it
  stops everything new for the whole check, assessment included: no
  admission, no chain extension behind the head, no background rebase of
  waiting merges, no resume. So the change is to split the gate: the
  assessment holds only the head's advance, and a dispatch adds the stop on
  new speculative work.

### Deploys that fail

- A deploy the serving daemon sees fail (build, install, rollout, a deploy
  that never owed its check, a released queue that does not advance within
  its bound) is a fact in the intake's next brief, and is dispatched like
  any other defect.
- A deploy after which no daemon serves cannot be seen by any intake.
  TODAY (verified 2026-11-14) the rollout already makes that impossible
  for a handover or a restart: a successor that does not prove it serves
  leaves the incumbent serving with the `successor_spawn_failed` fault
  open, and a restart that cannot finish takes its workspaces back rather
  than exit into nothing (`rollout/preflight.go`, `rollout/restart.go`).
  The serving daemon still owes the check, so the fault is a fact in the
  intake's brief.

### The sidebar

- `hot reloading` while the deploy rolls out; `assessing reload` while the
  intake checks it; `fixing reload` while a remediation is open.
- Each label is clickable: it selects the intake (hidden, shown for the
  selection) or the remediation workspace.

### Metrics

- The existing `agentrepl_deploy_mediation_seconds` outcomes become the
  intake's: `approved`, `escalated`, `dispatched`, `undecided`, `superseded`,
  `unmediated`.
- `agentrepl_deploy_remediation_seconds{outcome}`: each remediation, from
  dispatch to its fix's approval (`landed`) or its escalation, with its gate
  rounds in `agentrepl_merge_rounds`-style counts.
- The chain series stand as built.

## Owner rulings (2026-11-14)

1. A defect the deploy did not cause never holds the queue. It is still
   fixed: the intake files it as a one-shot through the queue, or as a todo
   row.
2. The bound is 4 remediations in a row: at the fourth's deploy still
   broken, the intake escalates instead of dispatching.
3. Models:
   - THE REMEDIATION: opus at medium effort. It researches, designs and
     fixes.
   - THE INTAKE: sonnet at low effort. It NEVER researches a cause or
     prescribes a fix: it states the problems that need remediating, each
     with its evidence (the records), and decides only approve, escalate,
     dispatch, and restore or fresh. The stronger model does the rest.

Because the intake is the weaker model, the daemon does the sorting it
would otherwise reason out, and hands it facts, not questions:

- The window's warnings and errors come grouped by `operation` and
  message, each group marked NEW (not seen in the same span before the
  deploy) or SEEN BEFORE (with its count then). Only NEW groups can hold the
  queue; SEEN BEFORE groups are filed (ruling 1).
- `--doctor`'s faults, and the latencies against the versions before, come
  as the same NEW or SEEN BEFORE facts.
- The chain so far: each earlier deploy, what the intake dispatched for it,
  the remediation's landed commits and the files they touched. The intake
  restores the last remediation when a NEW group's records name those files
  or that change, and creates a fresh one otherwise.
- The chain's count against the bound, so escalation at the bound is a rule
  it reads, not a judgement it makes.
- Test reuse (owner ruling, 2026-11-15): the last gate before the deploy, read
  from its log's runner lines (units total, reused, run, and why each ran:
  inputs changed, failed before, never reused, never recorded), against the
  20 gates before it. A gate of 100 or more units reusing under a quarter of
  their median rate is a NEW fact the intake escalates; the bounds were
  measured over 60 gates on 2026-11-15 (large gates 24-92%, median about
  45%, nine at 0%; gates under 100 units 0-46% with no pattern) and are
  stated at their site (`daemon/internal/merge/reusefacts.go`).

## Order of work

1. (Done) Verify what the hold stops today, and that a daemon that never
   comes up leaves one serving.
2. Proto and store: the intake and remediation roles on a chain, the hold's
   owner, the `dispatch` decision, the remediation's durable finish steps,
   the third label and its selection.
3. Daemon: the intake's lifecycle per chain; dispatch; the remediation's
   finish (gate and fix loop, direct landing, deploy, owed check); the
   speculative-work gate during a dispatch.
4. Briefs: the intake's (facts sorted NEW or SEEN BEFORE by the daemon)
   and the remediation's.
5. Sidebar: the third label, clickable labels.
6. Metrics and the catalog.
7. Retire the standing mediator and its brief.

## Implementation stages (contract landed 2026-11-14; every stage built 2026-11-14, deviations in the contract record's implementation notes)

The contract is in `docs/protobuf-design/2026-11-14-deploy-intake.md` and
the `.proto` files. Every stage below lands with the rest, on one branch:
the live daemon reads the doom repository's `deploy_mediator` config until
the daemon that understands `deploy_checks` is deployed with it.

1. (Done) Config: `lifecycleconfig` validates `deploy_checks`; the retired
   key fails to parse.
2. Store (wsm): the chain records its roles. `merge_queue_repos` gains the
   chain's intake workspace and its last remediation workspace, and the
   stage of the owed check (rolling out, assessing, dispatched/fixing). A
   dispatch is one write: the problems, the remediation chosen, the stage.
   The remediation workspace's direct landing records its steps (gated,
   landed, deployed) durably, keyed by workspace.
3. Hold split (`merge/standing.go` `queueHeld`): while the check is owed,
   the head neither lands nor is admitted; chain extension, background
   rebases and resumes are stopped only while the stage is dispatched.
4. Intake lifecycle (`merge/mediation.go`, `workspace/mediator.go`): the
   chain's intake is made hidden with a fresh context on sonnet at low
   effort when the chain's first deploy has rolled out, told of every
   deploy of its chain (no compaction), and closed when the chain ends.
   It is identified by its recorded role, never a configured name.
5. The intake's brief: the daemon sorts the window's WARN and ERROR records
   by operation and message into NEW and SEEN BEFORE (the same span before
   the deploy), adds `--doctor` faults and the latency comparison as facts,
   the chain so far (earlier deploys, dispatches, the remediation's landed
   commits and touched files) and the count against the bound. New brief
   `prompts/deploy-intake.md`; `prompts/deploy-mediation.md` and
   `metaprompt-deploy-mediator.md` are retired.
6. Dispatch (`DecideMediation`): validates the problems, enforces the
   bound and the restore refusal, then creates the remediation workspace
   (opus, medium effort, visible, fresh context, the problems and evidence
   as its prompt) or restores the chain's last one (reopen from its stored
   conversation; recreate its worktree on its branch at the main branch's
   tip; compact first when large; prompt it with only the new findings).
7. The remediation's finish (`FinishWorkspace` for a workspace whose role is
   remediation): answer `landing_directly`; run the full gate on its branch
   rebased onto the main branch; on a failure resume it with the failures
   (up to the queue's fixing bound, then the intake is told and escalates);
   on a pass land directly (the merge, changelog line and version tag
   `land-direct.sh` makes) and deploy; each step durable, resumed by the
   next daemon. The remediation workspace closes at the deploy; the deploy
   owes the chain's next check.
8. CLI: `agent-repld merge-queue mediation dispatch` with problems and
   restore or fresh; `merge-queue status` prints the stage.
9. Sidebar: the resolver draws waiting, checking, fixing with targets; the
   webapp draws `fixing reload` and makes the label a click target that
   selects (and shows, for the hidden intake) the workspace; Emacs decodes
   the arm and targets.
10. Metrics: the intake's outcomes (`approved`, `escalated`, `dispatched`,
    `undecided`, `superseded`, `unmediated`) and
    `agentrepl_deploy_remediation_seconds{outcome}`; the catalog.
11. The doom repository's `.agent-repl/lifecycle.yaml` moves to
    `deploy_checks: {remediations_before_escalation: 4}`; the standing
    `deploy-mediator` workspace is closed after the deploy.
