# Plan: a deploy in the footer (status, steps, activities, enduring blocked line)

Status: APPROVED 2026-10-01, to be implemented end to end without owner input. Protobuf changes
are pre-approved (no new rpc endpoints). No subagents in the implementing session.

## What stands today

- A deploy is drawn as ONE status-independent activity, `frontend.v1.FooterStatusActivityUpdate`,
  published on every workspace's strip through `deployprogress.Sink`
  (`daemon/internal/resolve/footer/update.go`).
- Its phases (`daemon/internal/deployprogress/deployprogress.go`):
  `building` (component list), `installing`, `restarting_services` (service list),
  `handing_over`, and the derived per-workspace `waiting` (turn and background counts).
- `updated` is a transient raised by whoever ends the story (this daemon, or the successor
  via `Manifest.Deploy`, `daemon/internal/rollout/progress.go`).
- A failure is the `deploy_failed` fault, and the activity line goes.
- There is no deploy status arm and no expanded panel.

## Principle: status versus activity (owner ruling, 2026-10-01)

- `deploying` is a STATUS, and it stands ONLY while THIS workspace has no working end-to-end
  path to the vendor because the deploy is moving something it runs on.
  - It is an unusable state, so it draws blue.
  - Another workspace's move, or a build that touches nothing running, is NOT this workspace deploying.
  - A lost vendor path from any OTHER cause (a dead shim, a severed link, a vendor outage) is NOT
    `deploying` either; it keeps its own status (`disconnected`, `blocked`).
- Everything a deploy does while the workspace still works stays an ACTIVITY on the
  workspace's own status (building, installing, waiting on its own work, deferred notes).
- `deploying` is exclusive with `working` and `merging` BY CONSTRUCTION: a workspace transfers
  only once it has drained (see "The per-workspace deploy"), so no turn or detached work runs during it.

## Every step, in order

### A. Daemon-wide (activity line, every strip)

| # | Step (arm) | Source today | Typed data |
|---|---|---|---|
| A1 | `building` | `ScriptBuilder.Build` steps | current target, done/total, over `proto shim webapp daemon store sidecar lock` |
| A2 | `installing` | `Deployer.install` | none |
| A3 | `waiting` (per workspace, derived) | the scheduled workspace's own live work | turn and detached counts, falling as the work drains |
| A4 | `reloading_webapp` | `Deployer.webapp` | none |
| A5 | `reloading_emacs` | `Deployer.elisp` | none |
| A6 | `rolling_back` | `Deployer.rollback` | none |

- The `restarting_services` and `handing_over` phases are RETIRED: store and sidecar are never
  restarted by a deploy, and a workspace's move is the `deploying` status (B), not an activity.
- Endings:
  - `updated` transient on success, as today;
  - a failure is a SALIENT activity naming the failed step (see "Expanded footer"), replacing
    the transient `deploy_failed` fault line.
- A blocked deploy (stale store or sidecar, protocol bump) raises the enduring blocked line and
  ends with no other step.
- A deploy asked for while another is in flight is never refused or lost: it goes through the
  deploy queue (see "The deploy queue").
- New producer plumbing: `deploy.Builder` gains a per-step progress callback (today it reports
  nothing until done), and A4 to A6 call `progress` where they only log today.
- Every step carries `stage_entered_at_ms`, the same field the merge bubble uses, so the client
  draws its duration.
- A FORCED deploy keeps its meaning: it does not wait for work (it is the user's explicit choice),
  so a forced transfer runs at once and ends the work in flight, as a forced bounce does today.
- A daemon whose state LAYOUT changes cannot hand over (the successor opens state read-only), so
  it is a daemon swap that waits until EVERY workspace has drained, then stops and starts; each
  workspace draws `deploying · transferring` from the stop until the new daemon serves it.
  - This is a `wsm schema version` change, which happens only for a BREAKING table change
    (`AGENTS.md`); additive table changes hand over normally, applied by the new daemon once it
    is the only writer.

### The deploy queue (owner design, 2026-10-01)

There are two queues: workspaces waiting on a deploy (see "The per-workspace deploy"), and
deploys waiting on a deploy, described here. Only master is ever deployed.

- A deploy asked for (a landing on master, or by hand) while one is in flight is scheduled, never
  dropped, so the newest code is always deployed in the end.
- It is scheduled with the NEW daemon of the deploy in flight, never the old one.
- A daemon starts a deploy only when it is fully deployed itself: no older daemon it supersedes is
  still around.
- Requests that pile up before a daemon can act collapse to ONE: the newest master commit wins, and
  the older ones are disregarded (ancestry is git's: `git merge-base --is-ancestor`).
- Mechanism (owner approved, over a post-merge git hook):
  - every build stamps the master commit it was built from;
  - a daemon, once it is the only one (its predecessor gone), compares its own stamped commit with
    master's head, and deploys if master is ahead;
  - a landing (the merge queue runs in the daemon and calls `rollout.Controller.Landed`,
    `daemon/internal/merge/terminal.go:630`) or a hand request says "master moved": the only
    daemon deploys; a daemon that is not yet the only one ignores it, because the check above
    runs when it becomes the only one;
  - the build is of the commit, not of whatever the master checkout holds uncommitted.
- So "the request goes to the new daemon" holds structurally: no request is carried at all, and
  the newest-commit-wins rule is the comparison itself.

### B. Per-workspace (status arm `deploying`, blue, only while this workspace is cut off)

See "The per-workspace deploy" below for the lifecycle. The substatus is ONE operation from the
client's side, whatever moves under the hood:

| # | Substatus | Stands while |
|---|---|---|
| B1 | `transferring` | from the old route's cleanup until the new route to the vendor serves the workspace (steps 3 to 6 below) |

- It carries the components moving (`daemon`, `shim`, or both) as typed data for the logs,
  but the status reads the same either way.
- The new daemon restates it, with its original start time, for a client that joins late.

## The per-workspace deploy (owner ruling, 2026-10-01)

### Scheduling

- A workspace is FREE when neither of these exists:
  - a turn connection to the workspace's current shim;
  - a detached-work connection to that shim (background agents, shells, monitors).
- A free workspace transfers at once.
- A busy workspace is SCHEDULED FOR A DEPLOY, and from that moment:
  - every prompt sent to it is enqueued as a held prompt classified `after deploy`
    (see "Held prompts");
  - no new USER prompt starts work, so the work begun before the schedule drains;
  - work the vendor starts on its own after the schedule (a new subagent, shell or monitor from a
    turn already running) is NOT held: it is waited on like the rest, because only the user's
    prompts are enqueued;
  - the moment it has fully drained, the transfer runs.
- An unknown live-work set is NEVER free.
  - GAP today: `promptqueue.queue.inFlight` (`daemon/internal/promptqueue/bounce.go:967`) reads a
    workspace with no watcher as free, and a successor judges freeness before it knows the
    adopted shim's live work.
  - Observed 2026-10-01 11:17:56: the old daemon recorded `detached_work: 1`, and one second
    later the successor recorded `the workspace is free; bouncing it now` and killed the shim
    with a background agent running.
- GAP today: holding begins only when a bounce RUNS (`bounce.go:650`), not when it is
  registered, so prompts sent while waiting become new turns and push the transfer back.

### The transfer: three supported combinations (owner ruling, 2026-10-01)

A deploy moves its services together or not at all, because a change to one may depend on the
others. A store or sidecar change blocks EVERY hot reload (see below). Only these combinations
are hot reloaded:

**Daemon only.** The shim is current, so its process is KEPT.

1. The new daemon is launched.
2. The old daemon tells Emacs and the webapp that a new daemon is up and that it is cleaning up
   this workspace's connections, so the clients expect the loss of the vendor path and do not
   report it as a fault or a blue disconnected status.
3. The old daemon severs its connection to the shim (the shim keeps running), then acks the
   clients that the cleanup is complete.
4. Emacs and the webapp connect to the new daemon.
5. The new daemon re-establishes the connection to the existing shim process.
6. The new daemon tells the clients that this workspace's deploy is complete.

- A free workspace runs this at once; a busy one is scheduled first (see "Scheduling").
- The old daemon exits once its last workspace has moved.

**Shim only.** The daemon stays, and the clients do not reconnect.

- The daemon shuts down the old shim, launches the new one, and connects to it.
- The clients are still told the transfer began and ended, so the footer, sidebar and tab bar
  show `deploying · transferring` for it.

**Daemon and shim.** The daemon-only sequence, with the shim swap as a daemon-side detail:

- at step 3 the old daemon shuts the old shim down instead of only severing it;
- at step 5 the new daemon launches the new shim and connects to it.

In every combination the client sees ONE transfer, from step 3 to step 6 (`deploying · transferring`).

- This CORRECTS a misrecorded ruling. The 2026-09-27 ruling was that a deploy does not wait on
  work the user tries to ADD while it is scheduled (those prompts are enqueued); it was recorded as
  "a handover never waits on work", and the mid-turn handover was built on that misreading.
  - A deploy ALWAYS waits for the work in flight when it was scheduled, and for anything the
    vendor starts from it.
  - The misreading is codified at `daemon/internal/rollout/handover.go:275` and in the rollout's
    tests and docs, and is corrected with this work.

## Held prompts (to be codified in `docs/USER-GUIDE.md` and `daemon/AGENTS.md`)

- Every held prompt lands in the feed's held-prompt section.
- Every held prompt has exactly one well-defined classification, drawn as its badge.
- A classification is either classifiable (a classifier decides when it goes) or not.

| Classification | Classifiable | Badge | Force |
|---|---|---|---|
| classifying | yes, deciding | | |
| interject | yes | | |
| after tool call | yes | | |
| after this turn | yes | red | sends now |
| after a context cut (uninterruptible turn) | no | red | none |
| after deploy (NEW, replaces build refresh and shutdown holds) | no | purple | asks first in the minibuffer |
| session starting | no | teal | none |

- Today `frontend.v1.HeldPrompt` carries the classification and the hold as two oneofs
  (`proto/src/frontend/v1/daemon_hold.proto`); codifying the table may fold them into one.
- The blank cells are filled from the current implementation when this is codified.
- Forcing an `after deploy` prompt asks in the Emacs minibuffer first, saying that sending it
  delays this workspace's deploy.
  - On yes, it is delivered, and the workspace stays scheduled until that turn drains too.

## The store and sidecar are never hot reloaded (owner ruling, 2026-10-01)

- They are shared by every workspace, so they cannot drain one workspace at a time; hot reloading
  them is not supported, which removes the problem instead of solving it.
- If the store or the sidecar is out of date FOR ANY REASON, NO deploy happens at all: nothing
  is installed, restarted, handed over or reloaded.
  - Out of date means the running service's build differs from the freshly built one
    (`Deployer.serviceStale`, `daemon/internal/deploy/deploy.go:689`); a deploy builds into
    staging, finds the difference, and stops before installing.
  - An unchanged service is never restarted.
- While blocked, the footer shows an ENDURING activity line,
  `Full emacs restart needed to unblock automatic agent-repl hot reloads` (see "Enduring lines").
- Required: a full Emacs restart builds and installs EVERY component, and starts each stale one
  on the current build: the store, the sidecar, the daemon, every shim and the webapp.
  - With working hot reloads the last three are never stale at a restart, but the restart is the
    recovery path when a hot reload breaks, so it covers them too.
  - The blocked line goes once every component runs the current build.

## Expanded footer: no deploy panel (owner ruling, 2026-10-01)

- A deploy is drawn ONLY in the footer's status, its steps and its activity line.
- A failed deploy raises a SALIENT activity naming the failed step, standing until the user's next
  EXPLICIT prompt.
  - A held prompt delivered automatically does not clear it.
  - It replaces today's transient `deploy_failed` fault line for this purpose.
- Every failure is also logged at ERROR with its step, detail and build-log path, as the
  deployer already does (`daemon/internal/deploy/{deploy,builder}.go`); the implementation
  confirms each failure path has its record.

## Client reloads: backends first, clients second (proposal)

- Clients are the webapp (one per webview) and the Emacs lisp (one per Emacs).
- ORDER: the backends (daemon, shim) always move first, and a client reloads only after
  the backend it talks to has moved.
  - Within one protocol version a new backend serves an old client, so the window between the two
    is safe; a breaking change is a protocol version bump, which is never hot reloaded (see below).
- Webapp: each workspace's webview reloads as the LAST step of that workspace's transfer, or at
  once when no backend changed.
  - A busy workspace's webview therefore waits with its transfer.
- Emacs lisp: ONE reload per Emacs, after every workspace's backend has moved (the old daemon has
  exited), or at once when no backend changed.
- Neither cuts the vendor path, so a client reload is an ACTIVITY (`reloading webapp`,
  `reloading emacs`), never the `deploying` status.
- So every deploy is a backend combination (none, daemon, shim, both) followed by the clients
  that changed (none, webapp, lisp, both).

## Deploys that are never hot reloaded

- A stale store or sidecar (above).
- A protocol version bump: any change of a proto package's version (`frontend.v1` to
  `frontend.v2`, say), which happens only on a breaking change.
  - Nothing records the versions a build speaks today: the build stamps them, and a deploy whose
    fresh set differs from the running one is blocked.
- Both block EVERY hot reload and raise the same enduring blocked line until a full Emacs restart.

## Enduring lines (owner ruling, 2026-10-01)

- The footer's enduring tier holds up to two lines:
  - the weekly and 5-hour usage line (today's only one);
  - the deploy-blocked line, only while hot reloads are blocked.
- With both present they alternate every 10 seconds, chosen by the wall clock rather than by a
  timer: line `floor(epoch_ms / 10000) mod 2`.
  - So when a transient or salient line above them goes away, the enduring line showing is the
    one the clock says, with the rest of its 10 seconds, never a fresh 10 seconds.
  - Every client, and every workspace's strip, shows the same line at the same moment.
- The daemon sends both lines; the client picks which to draw, because a choice made every 10
  seconds is the clock's, not a fact worth a frame.
- The sidebar shows no deploy notice (superseded).

## Emacs connections during a daemon swap

- While some workspaces have moved and others are still draining, Emacs holds TWO main
  connections, one to each daemon (`agent-repl-host-route-handover`, `lisp/host.el:1224`).
- "The old daemon has exited" means the old connection is gone and only the new one remains;
  that is when the Emacs lisp reloads.

## Implementation order

Land each as its own commits, tests with each (AAA, one edge case per test, error paths assert
the canonical log record), and keep the tracked suites green.

1. Proto (`frontend.v1`, regenerate with `make -C proto all`):
   - `FooterStatusDeploying` status arm with the `transferring` substatus and its activity cell;
   - `FooterStatusActivityUpdate`: retire `restarting_services` and `handing_over`, add the A steps
     with `stage_entered_at_ms`;
   - the failed-deploy salient activity (step and detail);
   - the enduring deploy-blocked line beside the usage line;
   - the held prompt's `after deploy` hold, replacing `build_refresh` and `shutdown` for deploys;
   - the client-facing transfer notices (steps 2, 3 and 6) on the existing daemon streams.
2. Daemon scheduling and holds: freeness never reads an unknown live-work set as free; a
   scheduled workspace holds user prompts as `after deploy` from the schedule; force asks
   (Emacs) and delays.
3. Daemon transfers: the three combinations; a current shim survives a daemon swap and is
   re-attached; the mid-turn handover and its misread ruling at `rollout/handover.go:275` go.
4. The wsm schema version rule: rename `LayoutVersion` and its docs and flags to the
   `wsm schema version`; additive migrations are applied by the new daemon once it is the only
   writer, and only breaking ones raise the version.
5. Deploy gating: a stale store or sidecar, or a protocol version change, blocks every hot reload;
   the build stamps its proto package versions.
6. Deploy queue: builds stamp and build from a master commit; the only daemon deploys on a
   landing or a hand request, and on becoming the only one when master is ahead of its commit.
7. Footer resolver: the status, activities, salient failure, enduring blocked line.
8. Webapp: the status, activity arms, durations, enduring alternation by wall clock, the purple
   `after deploy` badge.
9. Lisp: the transfer notices (no fault or blue status during an expected transfer), the force
   confirmation, the lisp reload after the old daemon's connection is gone.
10. Emacs restart path: build, install and start every stale component.
11. Docs: `docs/USER-GUIDE.md` (deploy footer, held prompts), `daemon/AGENTS.md` (held prompt
   classifications, the corrected drain rule), `docs/REMEDIATION-CHANGELOG.md`.

## Before starting

- DONE: `sessionwatcher.watcher.SetOutputAddress` and the `addr` sink parameters removed (`d6ff74611`).
- The first merge request FAILED (2026-10-01 14:34:34): the queue merged the branch recorded at
  workspace creation (`footer-activity-updates`, which no longer exists) instead of the checked-out
  `merge-queue-rework`, and the failure never reached this session. Owner-approved fixes:
  1. DONE (`29aa8c782`, `866fc7f0e`, skill `959b3cfa6`): own-branch, workspace and pr-merged
     requests record the branch CHECKED OUT in the worktree at request time; a detached HEAD is
     refused at once (`unknown_branch`); the `/merge-queue` skill says so and forbids requesting
     a branch that is not checked out.
  2. TODO, FIRST after compaction: a merge that fails (any area, including before its first
     step) automatically prompts the requesting workspace to analyze the failure.
     - A new `conversation.v1.PromptOrigin`, `PROMPT_ORIGIN_MERGE_FAILURE_ANALYSIS` (additive).
     - The daemon submits it once the failed merge has handed the workspace back, carrying the
       failure's area, summary, branch and the merge's log pointers.
     - It is NOT a merge-started origin, so its turn draws on the root feed, not the merge bubble.
     - The feed draws NO prompt row for it (as for `PROMPT_ORIGIN_VENDOR_STARTED`) but draws the
       turn's responses.
     - The store does NOT persist the prompt's words. The shim writes the turn-open prompt row
       as an EDGE with empty `said` (as a vendor-started row is), so the turn structure and the
       StartTurn ack still hold; the sidecar does not ingest the vendor transcript's record of
       that user message (it is recognized by a marker the shim puts on the submission).
       The vendor's own transcript file still holds it, which is outside our store.
- DONE: the merge of `merge-queue-rework` was re-requested after fix 1; do not edit the
  worktree while it runs.
- Known defect outside this plan, not yet ruled on: a user interrupt replaced the vendor process
  and killed a background agent (2026-10-01 13:03:29) while the shim logged that detached work
  keeps running.

## Questions for the owner

None open.
