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

## Principle: status versus activity

- A status arm says what THIS workspace is doing, and it is exclusive: a `deploying` status
  would hide `working`, `merging` or `background`.
- Most of a deploy does not touch the workspace: a turn runs normally while the daemon builds,
  installs, or restarts the store.
- So the plan splits it:
  - **daemon-wide steps stay an ACTIVITY** on whatever status the workspace has;
  - **the workspace's OWN move becomes a STATUS**, `frontend.v1.FooterStatusDeploying`, standing
    only while the workspace genuinely cannot serve (quiesced, transferring, shim replaced).
- This mirrors `frontend.v1.FooterStatusMerging`, which is a status only while the merge owns
  the workspace.

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

### B. Per-workspace (status arm `deploying`, only this workspace's strip)

| # | Substatus | Stands while |
|---|---|---|
| B1 | `waiting` | an unforced move waits on this workspace's own turn or background work |
| B2 | `quiescing` | the incumbent holds the workspace's intake and seals its queue |
| B3 | `transferring` | the workspace is handed to the successor |
| B4 | `adopting` | the successor claims serving and re-attaches the shim |
| B5 | `replacing_shim` | this workspace's stale shim is bounced and the fresh one starts |
| B6 | `taken_back` | the adoption window expired and the incumbent reclaims (then reverts) |

- B1 moves from the activity line to a substatus ONLY IF the owner rules so (question 1);
  - otherwise B1 stays the activity's `waiting` arm on a `working`/`background` status.
- B3 and B4 straddle the two daemons:
  - the incumbent's streams end at the transfer;
  - the successor must restate B4 for a client that joins late, so the manifest carries the
    step and its start time.
- Deferred notes (`shim_when_idle`) stay notes on the activity.

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

1. Should B1 (waiting on own work) be the `deploying` status, or stay an activity on `working`?
   - Recommended: stay an activity, because the workspace is still doing its own work.
2. Should a failed build row open the archived build log (`BuildFailed.Log`) when clicked?
3. Should the status color for `deploying` be purple (as merging is), or a new color?
