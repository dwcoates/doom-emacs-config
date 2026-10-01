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
- Everything a deploy does while the workspace still works stays an ACTIVITY on the
  workspace's own status (building, installing, waiting on its own work, deferred notes).
- `deploying` is exclusive with `working` and `merging`, because a status is one arm.
  - Gotcha: a handover never waits on work (`daemon/internal/rollout/handover.go:275`), so a
    transfer can happen mid-turn while the shim keeps the turn running.
  - For that window the strip draws `deploying · transferring`, and the turn's activity is not drawn.

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
| B1 | `restarting_store` | the store and sidecar restart, so no record path exists | every workspace |
| B2 | `restarting_sidecar` | the sidecar alone restarts | every workspace |
| B3 | `quiescing` | the incumbent holds this workspace's intake and seals its queue | the one being moved |
| B4 | `transferring` | this workspace is handed to the successor | the one being moved |
| B5 | `adopting` | the successor claims serving and re-attaches the shim | the one being moved |
| B6 | `restarting_daemon` | the stop-then-start layout restart has stopped this workspace | every workspace |
| B7 | `replacing_shim` | this workspace's stale shim is bounced and the fresh one starts | that workspace |

- `waiting` (an unforced move waiting on this workspace's own work) stays the activity's arm,
  because the workspace still works.
- B4 and B5 straddle the two daemons:
  - the incumbent's streams end at the transfer;
  - the successor restates B5 for a client that joins late, so the manifest carries the step
    and its start time.
- When the substatus ends, the workspace's own status returns (`working`, `idle`, ...).

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

1. While a turn is transferred mid-flight (B4), should the strip draw `deploying` and hide the turn,
   or should a transfer that carries a live turn stay `working` with a `transferring` activity?
   - The shim keeps the turn running, but no prompt can be sent until the successor adopts it.
2. Should a failed build row open the archived build log (`BuildFailed.Log`) when clicked?
