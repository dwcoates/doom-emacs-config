# daemon/internal/ssm/

The session-state manager: an in-daemon Go module (not a separate process)
owning the resolved per-workspace state (`thinking`, `idle`, `idle_async`,
`done`, `merged`, `merge_conflict`, …) in its own SQLite database
(append-only state log). It ingests lifecycle events forwarded from shim
streams plus daemon-local merge transitions, resolves current state via SQL
(precedence is a query, not a hardcoded ladder), and loud-logs every
transition with its causing event kind and seq.

This module replaces the Emacs-persisted workspace agent-state entirely.

## Composite session state

The SSM owns two independent current facts:

- Session connectivity: `hibernated`, `connecting`, `operational`,
  `degraded`, or `unavailable`.
- Session status: `ready`, `thinking`, `permission`, `done`, `interrupted`,
  `vendor-blocked`, or `monitoring`.

`session_connectivity` stores controller-generation lifecycle edges.
`session_fault` stores generation- and component-scoped fault windows.
`workspace_state` retains session-status, task, context-cut, merge, and legacy
projection history.

`operational` means the current controller generation passed ShimReady and has
no open connectivity-impacting fault. `degraded` is derived from an
operational lifecycle top plus at least one open connectivity fault for that
same generation. Retired generations and feature-, command-, or turn-terminal
faults cannot alter current connectivity.

Every connectivity and fault mutation carries the absolute workspace,
agent-repl session ID, controller-generation ID, stable component/fault key,
typed impact, and cause. An incomplete or stale identity is rejected loudly.
A close edge must match the exact open `(generation, component, fault_type)`
window; one component cannot clear another.

At daemon Open, every persisted controller generation is retired to
`hibernated`. A newly minted generation can become current only through
`connecting`; ShimReady advances that same generation to `operational`.
Hibernation is intentional and implies no fault. A terminal bring-up or
controller failure writes `unavailable`.

The frontend wire verdict contains connectivity, status, controller generation,
and active faults. The legacy render-state field is only the daemon's UX
projection and is not an authority from which clients may reconstruct either
dimension.

Historical `wired`, `starting`, `severed`, `dormant`, `degraded`, and
`degraded_clear` rows remain immutable evidence for older daemon revisions.
Once a controller-generation lifecycle exists, those rows have no authority
over composite connectivity.

## The vendor state is a turn OUTCOME, not a latch

`vendor_blocked` lives on the session-status lifecycle beside `done`, and the two are the
same kind of fact: a report of how the last turn ended. A `TurnEnded` writes
exactly ONE session-status lifecycle row — `vendor_blocked` when `VendorBlockingTurnEnd`
says the conclusion was abnormal, `done` otherwise. There is no vendor axis,
no clearing token, and no release entry point. Whatever the agent does next
supersedes the row, exactly as it supersedes `done`.

It was originally modeled as an independent latched axis, released only by a
subsequent clean `TurnEnded`. That form has no correct closed version:

- Some vendor blocks self-resolve with NO observable event. A usage limit
  resets on a clock the daemon cannot see, so requiring a witnessed release
  is wrong by construction.
- A session that died blocked could never emit the clean turn that released
  it, so the workspace stayed purple across every restart, forever.

Nothing gates on the state — prompts stay sendable while purple, and a retry
that hits the same wall just writes another `vendor_blocked` row. Rank 20 is
unchanged, so purple still sits between blue and red when it competes with
the merge, wired, backfill, and degraded axes; what changed is that a newer
session-status lifecycle row (`thinking`, `ready`, `done`, `dead`) now replaces it.

## The permission row is a covered turn, not a settled one

`permission` is an session-status lifecycle row like `done` and `vendor_blocked`, written by
`ApplyPermission` when the workspace's pending-permission count leaves zero and
released when it returns to zero. The count is derived from sessioncontroller's
`permRegistry` — one waiter per parked canUseTool round-trip — so a grant, a
deny and a teardown abandonment all close it without any of them being named.

What makes it unlike the other session-status lifecycle rows is that it BURIES a live
`thinking` rather than replacing a settled state. Two consequences follow, and
both are load-bearing:

- CLOSING reads the row beneath. `thinking` there means the turn that asked is
  still running and gets its row back; anything else is left alone, because
  appending a `thinking` the log has no evidence for is how a workspace wedges
  red with nothing able to release it.
- BOUNDING EDGES release it FIRST. A rotation abandons every waiter on the
  bounced shim, and `ApplySessionRotated` closes the row before its own
  turn-active check — otherwise the row hides the stuck turn that
  reconciliation exists to unstick. `warm()` does the same at Open: the
  rendezvous is in-process and does not survive a restart, while the shim does
  and re-asks on reattach.

A `TurnEnded` arriving over a pending permission needs no special case. Its row
is simply later, so it wins, and the close that follows finds nothing open.

Databases predating the remodel contain `vendor_clear` rows. No token maps
them to a render state and no CTE selects them, so they are inert and need no
migration.

## A merged workspace is a fact, not a row that happens to be newest

`workspace_merged` holds one row per workspace that has ever reached the
`merged` phase: the workspace and the instant it merged at. It is written by
`ApplyMergeTransition` at that transition and never rewritten — the primary key
is what makes set-once structural, so a second merge keeps the first landing.

It is deliberately NOT a query over the state log. The log answers "which merge
row is newest", which any later transition can change; "this workspace merged,
at this instant" becomes true once and stays true. The frontends were stripped
of merge state entirely and order their recently-merged section on this
instant, so re-deriving it would make that order depend on whatever happened to
the workspace afterwards.

It reaches the wire as `WorkspaceState.merged_at_ms` (unix millis, 0 = never
merged), stamped in `stampMergeFactsLocked` beside the queue facts — the one
WorkspaceState construction funnel, so a push, a snapshot and a synchronous
publication cannot disagree.

## merge_status is a COARSE projection of the merge axis, for now

`WorkspaceState.merge_status` (`mergestatus.go`) is the shape the merge
pipeline will report its run through: one message per phase inside a oneof, so
WHICH member is set IS the phase. It is stamped in `stampMergeFactsLocked`
beside the queue facts, which is what makes a frame carrying `merge_phase`
without a `merge_status` unrepresentable.

Until that pipeline exists, the projection is over the state log and nothing
else, so it reports only what the log knows:

- The phase, mapped coarsely — `merge_enqueuing`/`merge_queued` to `enqueued`,
  `merging` to `cherry_picking`, and the remaining tokens to their namesakes.
- The instant of the newest merge row, as BOTH `phase_started_at_ms` and
  `updated_at_ms`: the log records transitions and nothing else, so there are
  no within-phase ticks yet.
- The failure's cause, minus the `merge_transition:` prefix the log routes the
  row by.

Everything else — commit counts, the commit under test, the before/after
actions — stays at its zero value rather than being invented, and `run_id` is
an explicitly labeled PLACEHOLDER derived from the workspace and that instant.
Nothing may correlate on it until the pipeline mints a real one.

The older `merge_phase` / `merge_queue_position` / `merge_queue_depth` trio is
still stamped beside it. Both forms coexist until the cutover wave retires the
trio.

## The daemon stands a merged workspace down

Reaching `merged` also ends the workspace's session. `ApplyMergeTransition`
drives `ssm.MergedTeardown` (implemented by
`(*sessioncontroller.Manager).TeardownMerged`, which hibernates), bound through
`NewMergeLease` so the fleet that ran the merge is provably the fleet that
stands it down.

Two properties are load-bearing:

- The teardown runs AFTER the lock is released and AFTER the `merged` state has
  been handed to the subscribers. Every frontend state travels one ordered
  channel, so a frontend is told the workspace merged before anything the
  teardown produces can arrive — a merged workspace never simply disappears.
- A teardown failure NEVER travels back up `ApplyMergeTransition`. That error
  would reach `merge.Driver.finalizeMerged` and report a cherry-pick that
  already landed as a failed merge. It is surfaced through the canonical log
  instead, naming the session left running, exactly as a merge lease release
  failure is.

Dependencies: `proto/agentshim/` (generated Go), SQLite.
