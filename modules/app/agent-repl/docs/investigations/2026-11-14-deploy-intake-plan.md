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
3. THE REMEDIATION, at most one per chain. Created visible by the intake's
   dispatch, a child of nothing, with a fresh context and the intake's
   account of the defect as its prompt. Every later deploy of the chain that
   the intake finds broken is handed to the same remediation workspace while
   it is open, never to a second.

### The intake's decision

Each deploy's check ends in exactly one of:

- `approve`: nothing this deploy caused is wrong. The hold is released and
  the queue advances.
- `escalate`: something is wrong that should go before the user (no
  confident fix, or a user-facing choice). The hold is released, the user is
  notified, and the chain ends.
- `dispatch`: a defect THIS DEPLOY CAUSED, which an agent can fix. The hold
  stays, owned by the remediation workspace (below).
- Every defect the deploy did NOT cause (it predates the window's deploy, or
  it is noise of the rollout the harness already owns) does not hold the
  queue: the intake files it as a one-shot through the queue, or as a todo
  row, and decides the deploy on what remains. (Owner question 1.)

A chain whose remediation count reaches the bound (owner question 2) is
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

The remediation workspace closes when its fix's deploy is approved, or when
the chain is escalated.

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
  whatever is running finishes its step. (Verify first what the hold stops
  today.)

### Deploys that fail

- A deploy the serving daemon sees fail (build, install, rollout, a deploy
  that never owed its check, a released queue that does not advance within
  its bound) is a fact in the intake's next brief, and is dispatched like
  any other defect.
- A deploy after which no daemon serves cannot be seen by any intake. It is
  the deploy's own rollback that must restore a serving daemon, which then
  owes the check with the failure as its fact. (Verify first that a daemon
  that never comes up rolls back today.)

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

## Owner questions

1. Scope of the hold: may a defect the deploy did not cause be fixed
   without holding the queue (a one-shot through the queue, or a todo row)?
   Recommended: yes. It is what keeps the chain converging, and the defect is
   still fixed.
2. The bound on remediations in a row before the intake must escalate.
   Recommended: 3.
3. The intake's model: opus at medium effort, as the mediator runs.
4. The remediation's model: opus at medium effort.

## Order of work

1. Verify what the hold stops today, and that a daemon that never comes up
   rolls back.
2. Proto and store: the intake and remediation roles on a chain, the hold's
   owner, the `dispatch` decision, the remediation's durable finish steps,
   the third label and its selection.
3. Daemon: the intake's lifecycle per chain; dispatch; the remediation's
   finish (gate and fix loop, direct landing, deploy, owed check); the
   speculative-work gate during a dispatch.
4. Briefs: the intake's and the remediation's.
5. Sidebar: the third label, clickable labels.
6. Metrics and the catalog.
7. Retire the standing mediator and its brief.
