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
| A4 | `restarting_services` | `Deployer.services` | `store`+`sidecar`, or `sidecar` |
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
  - no new work starts, so the work begun before the schedule drains;
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

### The transfer: one operation from the client's side

With a new daemon AND a new shim:

1. The new daemon is launched.
2. The old daemon tells Emacs and the webapp that a new daemon is up and that it is cleaning up
   this workspace's connections.
   - The clients now EXPECT to lose the vendor path, so they do not report it as an unexpected
     fault or a blue disconnected status.
3. The old daemon disconnects from the old shim and shuts it down, then acks the clients that
   the cleanup is complete.
4. Emacs and the webapp connect to the new daemon.
5. The new daemon launches the new shim and connects to it.
6. The new daemon tells the clients that this workspace's deploy is complete.

- The client sees ONE transfer, from step 3 to step 6 (`deploying · transferring`).
- With only a new shim, the same daemon runs steps 3, 5 and 6, and the client's connection does not move.
- This REVERSES the 2026-09-27 ruling that a handover never waits on work
  (`daemon/internal/rollout/handover.go:275`): a workspace now always drains first.

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

## Expanded footer: yes, a `deploy` panel

- It earns a panel for the same reason merge tests do: several components progress in
  parallel states, and one line can name only the current one.
- Proposed `frontend.v1.FooterExpanded.deploy`, plus a `frontend.v1.FooterFocusDeploy` focus arm.
- One row per component, in the async rows' column layout (fixed duration column):

| Row | States |
|---|---|
| proto, shim, webapp, daemon, store, sidecar, lock | pending, building, built, failed |
| store, sidecar | up to date, restarting, restarted, failed |
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

1. When only the daemon is stale, should the shim still be replaced (one path, steps 1 to 6 always)?
   - Recommended: yes, because the workspace is drained, so the old shim holds nothing worth keeping.
   - It would delete the mid-turn adoption machinery (sealed moves, adoption windows, take-backs).
2. The store and sidecar are shared by every workspace, so they cannot drain one workspace at a time.
   - Should their restart wait until every workspace is drained, or can store clients reconnect
     across the restart without losing the vendor path?
3. Should a failed build row open the archived build log (`BuildFailed.Log`) when clicked?
