# The vendor's activity level: the shim is the sole stand-down authority

Deploys lost detached work. When a shim is owed a replacement (its skill set
went stale, or its build), the daemon's bounce registry waits for the
workspace to be free and then asks the shim to stand down. The daemon judged
"free" from the turn in flight and the live-work level, and so did the shim's
own `KillSession` guard. Neither sees the turn the vendor starts on its own the
moment a background subagent concludes: the vendor queues the notification and
runs a turn to hand the result to the main agent, and nothing on the turn or
live-work planes says so until that turn's first reply. The stand-down landed
inside that window and the hand-back was lost (resumed as `status="stopped",
restarted=true`). The owner ruled that the shim is the sole authority on
whether its session may stand down, and that pending wakeups and crons count
as work.

## Core design principles

### The shim is the sole authority on stand-down

- **The principle, in the owner's terms**: the shim is the sole authority on
  whether its session can be stood down.
- **Consequences for the contract**: an unforced `shim.v1.KillSession` is
  refused unless the shim judges the session idle, and that judgement includes
  the vendor's own account of itself, not only what the shim has seen start.
  The daemon's freeness reading is advice for when to ask; the refusal is the
  answer, and the daemon must be told when the shim's answer changes so it can
  ask again.
- **Decisions it reopens**: none landed in the contract. It confirms the
  existing daemon behavior that a `SessionLive` refusal re-registers the
  replacement (`promptqueue.deferReplacementLocked`).
- **What it does not claim**: it does not make forced stand-downs (a forced
  deploy, `UpdateShutdownSchedule{now}`) wait; force still ends work, by the
  caller's order.

### Pending wakeups and crons are work

- **The principle**: a session with a scheduled wakeup or cron job pending is
  not free.
- **Consequences**: carried by the vendor's own idle state, which is busy while
  any is pending, so no new detached-work kind is needed.
- **What it does not claim**: it does not ask for wakeups or crons to be drawn
  anywhere.

## Assumptions (autonomous mode)

- Objective: no unforced stand-down ever ends or loses work.
- Systems: `conversation.v1` (contract), the shim (producer and the stand-down
  authority), the daemon's session watcher (consumer: freeness). No frontend
  component; nothing new is drawn.
- Non-additive changes: the shim's `KillSession` refuses in a state it used to
  accept (vendor busy with nothing seen). Older shims keep the old guard; the
  contract revision tells the daemon which it is talking to.
- Evidence tiers: the vendor binary's own code (observed, in
  `claude-agent-sdk-darwin-arm64/claude` v2.1.311) emits
  `system/session_state_changed` with `idle | running | requires_action` only
  when `CLAUDE_CODE_EMIT_SESSION_STATE_EVENTS` is set (otherwise the SDK sets
  `CLAUDE_CODE_SDK_READS_SESSION_STATE` and the frames carry `sdk_host_only`,
  which the SDK swallows). Its idle predicate (`kb`) is false while background
  tasks run (`CLAUDE_CODE_BG_TASKS_REPORT_RUNNING` defaults on), a
  notification is pending, or teammates are active, and the wait loop stays
  non-idle while a session wakeup is pending (`hasPendingSessionWakeup`, over
  the session cron task list). The SDK typing (declared) calls idle the
  "authoritative turn-over signal".

## Iteration sequence

Default, walked alone: no transport change; no endpoint change; the outcome
convention is untouched (the refusal arm already exists); one increment in
`conversation/v1/session.proto`.

## Landed changes

### 1. `conversation.v1.SessionVendorActivity` and its three use sites

- **What**: a new level message with `idle` and `busy` arms, carried as
  `SessionUpdate.vendor_activity` (41), `SessionStarted.vendor_activity` (12,
  optional) and `SessionLive.vendor_activity` (3, optional); a new contract
  revision `SESSION_CONTRACT_VENDOR_ACTIVITY_LEVEL = 2`.
- **Why**: the vendor's state is the only fact that is busy across the window
  between a background task concluding and the hand-back turn's first reply,
  and across a pending wakeup or cron.
- **Why two arms, not the vendor's three**: `requires_action` is an ask, which
  the ask plane already carries; for stand-down it is simply busy. A third arm
  would be a second account of the ask.
- **Why not a new detached-work kind for wakeups and crons**: the vendor's
  idle already covers them, and a kind would need the shim to track the
  vendor's timers itself, a second authority over the same fact.
- **Unknown is busy**: a statement with no arm set is read as busy, so schema
  skew can never permit a stand-down.
- **Consequences**:
  - The shim must set `CLAUDE_CODE_EMIT_SESSION_STATE_EVENTS=1` on the vendor
    it spawns, track the state, push it on change and on every
    `WatchSession` open, and add it to the `KillSession` guard.
  - The daemon's `sessionwatcher.freeLocked` must count a busy vendor as not
    free; the move to idle becomes a freeness edge (`OnFree`), which is what
    re-runs a replacement the shim refused.
  - Accepted: a session whose vendor reports busy for a long time (a
    persistent monitor, a cron) is never replaced by an unforced deploy until
    it goes idle. That is the ruling, not a cost to engineer around.
