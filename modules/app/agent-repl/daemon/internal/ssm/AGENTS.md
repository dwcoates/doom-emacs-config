# daemon/internal/ssm/

The session-state manager: an in-daemon Go module (not a separate process)
owning the resolved per-workspace state (`thinking`, `idle`, `idle_async`,
`done`, `merged`, `merge_conflict`, …) in its own SQLite database
(append-only state log). It ingests lifecycle events forwarded from shim
streams plus daemon-local merge transitions, resolves current state via SQL
(precedence is a query, not a hardcoded ladder), and loud-logs every
transition with its causing event kind and seq.

This module replaces the Emacs-persisted workspace agent-state entirely.

## The paint axis is a latch that re-arms

Green promises two things: the route is usable AND a frontend has attested
drawing the history. The second half is the paint axis, and it has three
edges rather than one:

- OPENS when a session asserts readiness (`Apply`, alongside `ready`). A new
  route has been attested by nobody, so the workspace is blue until someone
  draws it. Without this edge the axis contributed no candidate at all for a
  workspace that had never been acked, and the documented blue gate never
  engaged — a workspace resolved green with no paint row in the log.
- CLOSES on `ApplyPaintAck`, and only for a render that actually DREW. A
  suspended webview's acknowledgment settles that state's delivery to Emacs
  and attests nothing here.
- RE-OPENS on `ApplyPaintLost`, called by the frontend server when a painting
  connection attaches or leaves. Attestation is a claim by one renderer about
  one connection, so a renderer that is gone cannot keep a workspace green.

`paintWatermark` deliberately drops the seq on withdrawal, so a re-attestation
after a break starts from nothing rather than inheriting the pre-break seq.

## The vendor state is a turn OUTCOME, not a latch

`vendor_blocked` lives on the AGENT axis beside `done`, and the two are the
same kind of fact: a report of how the last turn ended. A `TurnEnded` writes
exactly ONE agent-axis row — `vendor_blocked` when `VendorBlockingTurnEnd`
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
the merge, paint, backfill, and degraded axes; what changed is that a newer
agent-axis row (`thinking`, `ready`, `done`, `dead`) now replaces it.

## The permission row is a covered turn, not a settled one

`permission` is an agent-axis row like `done` and `vendor_blocked`, written by
`ApplyPermission` when the workspace's pending-permission count leaves zero and
released when it returns to zero. The count is derived from sessiondrv's
`permRegistry` — one waiter per parked canUseTool round-trip — so a grant, a
deny and a teardown abandonment all close it without any of them being named.

What makes it unlike the other agent-axis rows is that it BURIES a live
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

Dependencies: `proto/agentshim/` (generated Go), SQLite.
