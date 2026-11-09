# A run's wait for a RAM disk seat, in the merge bubble's tests tab

A merge-queue gate's test run can wait minutes for a RAM disk seat before its
runner plans a single suite (2026-11-09: 2m10s behind ordinary workspace runs).
The tests tab showed nothing but an empty run while it waited. The owner ruled
(2026-11-09) that the tab must show the wait instead, naming the holders by
workspace name, e.g. "waiting for a RAM disk seat (held by <workspace>) 1m 40s".

Designed in autonomous mode.

## Assumptions (autonomous mode)

- Objective: the merge bubble's tests tab draws a run's pre-plan wait for a RAM
  disk seat, with who holds the seats and for how long it has waited.
- Frontend component: yes, the tests view of a run subtab
  (`frontend.v1.FeedMergeTestsRunOutput`), so figma→idl governs.
- Systems: testrun (writes the wait to its run log), the daemon's merge gate
  (reads the runner's lines, as it already does for every suite edge, and
  resolves the view), the webapp (draws it).
- Non-additive changes: none to the contract. The daemon's test commands gain an
  environment variable naming the requesting workspace, so the runner can write
  its name into the seat it holds.

## Iteration sequence

One increment: the seat wait's view, as one optional element of the run's
tests view.

## Landed changes

### `frontend.v1.FeedMergeTestsRunOutput.seat_wait`

- WHAT: an optional element of a run's tests view, set while the runner waits
  for a RAM disk seat, carrying when the wait began (the daemon's first reading
  of it), why (a oneof: every seat held under the ceiling, the memory floor, or
  a better-ranked run waiting first), and the holders, each a workspace name or,
  for a run started by hand, its pid.
- WHY: the owner wants a gate's pre-plan wait visible instead of an empty tab,
  holders named by workspace, never by ID.
- Placement: on the tests VIEW (`FeedMergeTestsRunOutput`), not as a new arm of
  the run's `state` oneof. The run is live (it holds its repository's test turn
  and its runner is running); the seat wait is content of the live run, and
  replacing the live arm would hide the run's own elapsed badge and its log link.
- Reasons are arms, not an enum: the memory reason alone carries the available
  memory and the floor, and the ceiling reason alone carries the ceiling.
- Holder identity is a oneof (workspace / outside): a run started by hand has no
  workspace, and naming it by a blank string would be a sentinel.
- Consequences: the runner's wait line is a contract between testrun and the
  daemon. It is defined ONCE, in a testrun package the daemon imports
  (`agentrepl/testrun/seatwait`), so the line's format and its parse cannot
  drift apart. The daemon reads lines, not `--events-fd` events, because the
  merge gate reads the runner's lines for every other edge it draws; the
  agent-facing `agent-repld test run` stream gains no seat event in this change.
- Accepted cost: `started_at_ms` is the daemon's first reading of the wait, so a
  daemon restart mid-wait restarts the clock.
