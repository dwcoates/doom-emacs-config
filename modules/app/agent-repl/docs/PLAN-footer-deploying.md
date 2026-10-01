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
- `deploying` is exclusive with `working` and `merging`, because a status is one arm.
  - Gotcha: a handover never waits on work (`daemon/internal/rollout/handover.go:275`), so a
    transfer can happen mid-turn while the shim keeps the turn running.
  - For that window the strip draws `deploying · transferring daemons`, and the turn's activity is not drawn.

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

| # | Substatus | Stands while | Which workspaces |
|---|---|---|---|
| B1 | `transferring_store` | the store (and with it the sidecar) is swapped for the fresh build | every workspace |
| B2 | `transferring_sidecar` | the sidecar alone is swapped | every workspace |
| B3 | `transferring_daemon` | from the moment the old daemon stops taking this workspace's prompts until the new daemon serves it (a handover, or the stop-then-start a layout change needs) | the one being moved |
| B4 | `transferring_shim` | from the moment the stale shim is stood down until the fresh shim serves the session | that workspace |

- One verb for every component (owner ruling, 2026-10-01): from the user's side each is the same
  thing, the workspace moving from an old process to a new one, because no work is ever interrupted.
  How each swap works (a handover, a prelaunched shim, a launchd restart) stays in the logs.
- `waiting` (an unforced move waiting on this workspace's own work) stays the activity's arm,
  because the workspace still works.
- Each step is ONE period, because the workspace cares about the gap, not about its two ends.
  - Inside it the old daemon pauses intake, hands off, and the new daemon reconnects the shim.
  - Those edges are logged, and a stall at either one is drawn by the fault line it already has
    (a refused workspace taken back, an expired reconnect window), never by a separate substatus.
- B3 straddles the two daemons:
  - the old daemon's streams end mid-period;
  - the new daemon restates B3, with the period's original start time, for a client that joins
    late, so the manifest carries that start time.
- When the substatus ends, the workspace's own status returns (`working`, `idle`, ...).

## Required mechanics (owner ruling, 2026-10-01)

The status is cosmetic only if the deploy behaves like this, so these are part of the work.

1. **A shim transfer starts only once the workspace is free**: no turn in flight and no detached
   work (background agents, shells, monitors) still running.
   - The gate exists: `promptqueue.queue.inFlight` (`daemon/internal/promptqueue/bounce.go:967`).
   - GAP: a workspace with no watcher reads as free, and a successor daemon judges freeness before the
     adopted shim's live work is known. Observed 2026-10-01 11:17:56: the old daemon recorded
     `detached_work: 1`, and one second later the successor recorded `the workspace is free;
     bouncing it now` and killed the shim with a background agent running.
   - Fix: freeness is judged only after this daemon holds the shim's live-work facts (a latch the
     judgment awaits, as `awaitReattachedFacts` does for a reclaim). An unknown live-work set is never free.
2. **Every prompt that arrives after a transfer is scheduled is held, unclassified, until the
   transfer is done.**
   - GAP: holding (draining) begins only when the bounce RUNS (`bounce.go:650`), not when it is
     registered to wait for freeness, so prompts sent while waiting are delivered as new turns and
     keep pushing the transfer back.
   - Fix: registering a deploy's bounce holds the intake at once; the classifier never runs on those prompts.
3. **Such a held prompt shows the badge `after deploy`, purple**, instead of `after this turn` (red).
   - Today a shim swap and a daemon handover both hold with `HoldBuildRefresh` (badge `build refresh`,
     amber) and a scheduled shutdown with `HoldShutdown` (badge `restart hold`, amber),
     `daemon/internal/resolve/holds/tray.go`.
   - Fix: a deploy's holds draw `after deploy` in purple, the color that already means a merge or
     deploy is moving the workspace.

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

1. While a turn is mid-flight during B3, should the strip draw `deploying` and hide the turn,
   or should a transfer that carries a live turn stay `working` with a `transferring daemons` activity?
   - The shim keeps the turn running, but no prompt can be sent until the successor adopts it.
2. Should a failed build row open the archived build log (`BuildFailed.Log`) when clicked?
