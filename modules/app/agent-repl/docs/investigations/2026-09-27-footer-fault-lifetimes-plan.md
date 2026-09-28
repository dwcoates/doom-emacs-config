# Deferred plan: every footer fault has a defined end

Status: DONE (2026-09-28, branch `feat/footer-fault-lifetimes`). Approved by
the owner on 2026-09-28 after being deferred on 2026-09-27. The table is
`daemon/internal/health/lifetime.go`, the one close function is
`daemon/internal/health/faultclose.go`, and the guard is
`daemon/internal/health/lifetime_test.go`. See "As landed" at the end.

## The symptom

The footer's activity line showed `kind:"bounce_unknown" detail:"the bounce
meant to unattested this session; its workspace lock reads held"` for over 30
minutes.

## How the activity line changes today

- MOMENTARY lines (context injected, notifications) retire themselves after a
  short dwell, about 1.5 s in the logs (`daemon.footer.retire_momentary`).
- TURN lines (compaction, a running hook, retrying, "stopping the current
  turn…") end at the turn's terminal or their own end signal.
- IDLE lines (rate limit, context budget) stand while no turn runs.
- FAULT lines outrank everything (`activity.go` `faultLine`).
  - A fault is a durable `wsm` record (`OpenFault`).
  - The footer drops its line only when something calls `CloseFault` for that
    record (`resolve/footer/faults.go`).

## Root cause

Several fault kinds have NO closing edge anywhere in the daemon.

- `bounce_unknown` is opened at a restart's reconcile (`rollout/manifest.go`
  ~530). That code closes only the ordinary dispositions (preserved, rolled,
  died) right after recording them (~548).
- A healthy attach closes only `shim_died` and `link_severed`
  (`workspace/sessions.go:1480`, `linkFaultKinds`).
- A session start closes only `resume_failed` (`closeSessionRefusedFaults`).
- `adoption_window_expired` (opened on five workspaces at 14:47:37 on
  2026-09-27) has no closer either.
- So these lines stand until the next daemon restart, whether or not the
  workspace recovered.

## The fix

1. Every fault kind declares its lifetime in ONE table in the `health`
   package, next to the existing kind-to-footer-cell table (`health/footer.go`).
   Each kind is either:
   - STANDING until a named recovery edge: a healthy attach, a session start,
     the next served turn, an owner action, and so on;
   - or MOMENTARY: an event report shown briefly and closed at once, like the
     ordinary bounce dispositions.
2. The recovery edges close every standing fault whose declared edge they are,
   through one function, instead of each call site listing kinds by hand
   (today's `linkFaultKinds` and the `resume_failed` special case).
3. `bounce_unknown` and `adoption_window_expired` close when the workspace
   next attaches or starts healthy.
4. A table test enumerates every kind in `health/kinds.go` and fails if any
   has no declared lifetime, or if a declared edge has no caller.
5. Every close is logged at INFO with the kind, the fault id, the edge that
   closed it, and how long it stood.
6. Tests, table-driven AAA:
   - each standing kind closes on its edge and not on others;
   - a momentary kind never stands;
   - the footer line clears when its fault closes;
   - the guard test.

## Related, NOT part of this plan (the owner has not ruled on it yet)

- The 14:47:37 handover whose successor did not claim five workspaces within
  the 30 s adoption window. That is a handover defect in its own right.

## As landed (2026-09-28)

- Every kind in `health/kinds.go` (plus `cold_gate_reopen_failed` and the
  `bounce_disposition` accounting kind, now `health.KindBounceDisposition`)
  has one row. Kinds added to master after the plan are covered:
  - `deploy_failed` stands until the step's next success, a newer failure of
    the same step (superseded), or a daemon boot;
  - `successor_spawn_failed` stands until a successor proves it is serving.
- Kinds the reporter DERIVES per answer and never records
  (`session_absent`, `daemon_state_unreadable`) are declared MOMENTARY: there
  is no record to stand.
- `log_sink_poisoned` and `wsm_read_only` are opened by nothing today; they
  are declared standing until a daemon boot, the next process opening its own
  handle and sinks.
- `conversation_abandoned` and `classifier_failed` stand until the next turn
  the daemon OPENS; a turn replayed from history retires only the final-answer
  fault.
- The healthy-attach edge fires at `Fleet.Start`'s bring-up, a relaunch's
  installed replacement, a successor's dialed adoption, and the boot's own
  adoptions AFTER the manifest reconcile. The last one is a judgement call:
  the boot adopts before it reconciles, so an undetermined bounce recorded for
  a workspace the boot already adopted is closed at once (recorded, WARN, then
  closed at INFO). That is the 30-minute symptom above; the integration test
  that asserted the fault stood open on the host stream was amended to assert
  the record and its close.
