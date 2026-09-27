# Deferred plan: every footer fault has a defined end

Status: DEFERRED by the owner (2026-09-27). This is a more involved fix, to be
picked up once the smaller items in flight have landed. Tracked in
`2026-09-23-agents-in-flight.md` under "Deferred".

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
