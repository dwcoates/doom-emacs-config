# Plan: a deploy in the footer (status, steps, activities, expanded panel)

Status: PROPOSED, not built. Owner rulings needed on the questions at the end.

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
| A1 | `requested` | `Deployer.Deploy` entry | forced or not |
| A2 | `building` | `ScriptBuilder.Build` steps | current target, done/total, over `proto shim webapp daemon store sidecar lock` |
| A3 | `installing` | `Deployer.install` | none |
| A5 | `reloading_elisp` | `Deployer.elisp` | count of Emacs clients told |
| A6 | `handing_over` | `rollout.HandOver` | workspaces moved / total |
| A6' | `restarting_daemon` | `rollout.Restart` (layout change) | running and fresh layout |
| A7 | `replacing_shims` | `Deployer.shims` | replaced now / deferred to idle |
| A8 | `reloading_webviews` | `Deployer.webapp` | none |
| A9 | `rolling_back` | `Deployer.rollback` | what is being restored |

- Endings stay as they are:
  - `updated` transient on success;
  - `deploy_failed` fault on failure, now also naming the failed step and whether the rollback finished.
- Refusals (`ErrAlreadyDeploying`, `rollout.ErrJoining`, `ErrAlreadyRollingOut`) raise a
  transient, because today only the rpc caller hears them.
- New producer plumbing:
  - `deploy.Builder` gains a per-step progress callback (today it reports nothing until done).
  - A5, A7, A8 and A9 call `progress` where they currently only log.
- Every step carries `stage_entered_at_ms`, the same field the merge bubble uses, so the client
  draws its duration.

### B. Per-workspace (status arm `deploying`, blue, only while this workspace is cut off)

See "The per-workspace deploy" below for the lifecycle. The substatus is ONE operation from the
client's side, whatever moves under the hood:

| # | Substatus | Stands while |
|---|---|---|
| B1 | `transferring` | from the old route's cleanup until the new route to the vendor serves the workspace (steps 3 to 6 below) |

- It carries the components moving (`daemon`, `shim`, or both) as typed data for the expanded
  panel, but the status reads the same either way.
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
- A deploy still detects when either is out of date: their build differs from the fresh one
  (`Deployer.serviceStale`, `daemon/internal/deploy/deploy.go:689`). An unchanged service is never restarted.
- When either is out of date, EVERY automatic hot reload (daemon, shim and webapp included) is
  BLOCKED until a full Emacs restart.
  - The sidebar shows, at its very bottom with a small gap from the edge, in red:
    `Full emacs restart needed to unblock automatic agent-repl hot reloads`.
  - The text is sized to fit on one line of the sidebar and never wraps.
  - It stands until the restart has brought the stale service onto the fresh build.
- Required: a full Emacs restart actually restarts a stale store or sidecar onto the installed build.

## Expanded footer: yes, a `deploy` panel

- It earns a panel for the same reason merge tests do: several components progress in
  parallel states, and one line can name only the current one.
- Proposed `frontend.v1.FooterExpanded.deploy`, plus a `frontend.v1.FooterFocusDeploy` focus arm.
- One row per component, in the async rows' column layout (fixed duration column):

| Row | States |
|---|---|
| proto, shim, webapp, daemon, store, sidecar, lock | pending, building, built, failed |
| store, sidecar | up to date, out of date (Emacs restart needed) |
| daemon | up to date, handing over, restarting, handed over |
| shim (this workspace) | up to date, replacing, when idle |
| webapp | up to date, reloading |

- The row dot reuses the work-dot colors: running filled green, failed filled red, done hollow.
- The panel stays visible until the `updated` transient expires, so the final result can be read.
- The panel's state must survive the handover, so the manifest carries the row table.

## Work, by system

1. Proto (`frontend.v1`): new status arm and substatuses, new activity arms, the panel and focus arm.
2. Daemon:
   - `deployprogress.Progress` grows the steps and per-component rows;
   - the builder callback;
   - the resolver derives the B substatuses from the rollout's per-workspace transfer state;
   - the manifest carries steps and rows across the handover.
3. Webapp: the status, activity arms, panel rows and durations in `footer/`.
4. Lisp: nothing beyond the generic status rendering, to be confirmed.
5. Docs: `docs/USER-GUIDE.md` gains the deploy footer, per the `AGENTS.md` rule.

## Questions for the owner

1. Should a failed build row open the archived build log (`BuildFailed.Log`) when clicked?
