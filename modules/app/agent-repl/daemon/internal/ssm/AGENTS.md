# daemon/internal/ssm/

The session-state manager: an in-daemon Go module (not a separate process)
owning the resolved per-workspace state (`thinking`, `idle`, `idle_async`,
`done`, `merged`, `merge_conflict`, …) in its own SQLite database
(append-only state log). It ingests lifecycle events forwarded from shim
streams plus daemon-local merge transitions, resolves current state via SQL
(precedence is a query, not a hardcoded ladder), and loud-logs every
transition with its causing event kind and seq.

This module replaces the Emacs-persisted workspace agent-state entirely.

## Attestation is GONE; the axis that replaced it is WIRED

Green once promised two things: the route is usable AND a frontend has attested
drawing the history. The second half was the paint axis — an `unpainted` token
opened by readiness, closed by a webview's paint acknowledgment, re-opened when
a painter connected or left. It is deleted, tokens, rank row, watermark and
entry points alike.

A workspace's color is CONNECTION TRUTH. Blue and teal both mean there is no
live backend session for this workspace; every OTHER color is a GUARANTEE that
the session substrate is fully wired. What a viewer has or has not drawn says
nothing about that, so it can no longer hold a workspace blue.

The WIRED axis is the fact that does answer the question, and sessioncontroller
produces it (see internal/sessioncontroller/wiredstate.go):

- `wired` — the bring-up gate CLOSED for this workspace: the shim answered
  ShimReady, which is the same verdict `AwaitReady` resolves on. The axis
  contributes no candidate, so the agent axis and the vendor outcome are what
  the workspace renders.
- `starting` — a bring-up is actively in flight. Blue, rendering
  RENDER_STATE_INIT, the same word bring-up has always had.
- `severed` — not wired, not starting, and there is EVIDENCE THAT SOMETHING
  BROKE: a bring-up that could not be completed, or a session controller whose `client.Run`
  returned a terminal protocol error. Blue, rendering RENDER_STATE_SEVERED.
  Rank 12.
- `hibernated` — not wired, not starting, and NOTHING IS WRONG: the shim was
  SIGTERMed on purpose to reclaim its ~500MB, or nothing has ever been wired
  here. TEAL, rendering RENDER_STATE_HIBERNATED. Rank 15.
- `dormant` — the PRE-SPLIT spelling of the closed half, and load-bearing
  forever. `workspace_state` is append-only, so rows carrying this literal text
  still exist and must keep resolving; it is an alias for `severed`, ranked
  identically at 12. Never migrate, rewrite or delete those rows.

THE CLOSED HALF IS TWO TOKENS, AND IT USED TO BE ONE. `dormant` meant both
"asleep on purpose" and "the substrate is broken", so the idle sweeper reclaiming
memory from an untouched workspace rendered exactly like a dead shim. A color
that fires on both means neither, which is why blue was worth nothing until the
split. Now blue means something is actually wrong.

TEAL'S RANK IS THE LOAD-BEARING PART. `hibernated` sits directly below the blue
band and above purple, NOT below green, because hibernation makes the SAME
actionability claim blue does — you cannot interact without paying a bring-up —
and only its REASON is benign. Below green, a stale `thinking` row from the turn
a workspace was hibernated after would mask a workspace that is genuinely
asleep. That combination is therefore unreachable by construction, and a
resolver that ever detects it must log an INVARIANT VIOLATION rather than
treating it as expected.

ABSENCE OF A ROW IS HIBERNATED, not "unknown" and not "severed". A workspace with
agent-axis rows and no wired row at all has no evidence of a live session behind
it, and inventing one from the agent axis is precisely the lie the law forbids —
but it has no evidence of a BREAKAGE either, so the honest answer is teal. The
resolution query synthesizes the `hibernated` candidate for that case rather
than letting the agent axis answer unopposed.

`warm()` CLOSES the axis for every restored workspace, the same way it releases
persisted permission rows and for the same reason: the wiring is in-process and
does not survive a restart, while the agent-axis history does. It closes to
`hibernated`: a daemon that has just booted is not a broken one, and the
bootsweep is about to reattach every shim that survived the bounce.

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
the merge, wired, backfill, and degraded axes; what changed is that a newer
agent-axis row (`thinking`, `ready`, `done`, `dead`) now replaces it.

## The permission row is a covered turn, not a settled one

`permission` is an agent-axis row like `done` and `vendor_blocked`, written by
`ApplyPermission` when the workspace's pending-permission count leaves zero and
released when it returns to zero. The count is derived from sessioncontroller's
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
