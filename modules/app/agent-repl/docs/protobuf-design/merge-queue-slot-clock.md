# Merge queue slot clock

Owner request (2026-11-03): in the sidebar's Merge Queue section, the duration
shown on every ACTIVE merge (the head, a merge racing behind it, a merge
remediating out of line) must be exactly how long it has held its CURRENT
active slot. A head that steps aside to remediate starts its remediating clock
at zero; one that returns and takes the head again starts the head clock at
zero again; a merge taking the head for the first time starts at zero. A merge
read 1h 36m while testing at the head because the head clock came from
in-memory state that a resumed run carried across slots and restarts.

## Assumptions (autonomous design)

- Objective: one durable fact per queue entry, "when it took its current
  slot", read by the roster for every active slot.
- Frontend component: yes, the sidebar's Merge Queue section (figma→idl).
- Systems: daemon (wsm store, merge orchestrator, sidebar resolver), webapp
  sidebar, Emacs wire decoder.
- Non-additive changes: none on the wire. The daemon drops its in-memory head
  clock (`run.headMS`, the orchestrator's `waitingHeads`, the progress
  record's `HeadMS`).

## Iteration sequence

Default sequence; the change is one increment on one component
(`frontend.v1.RosterRepoMergeQueue`'s rows).

## Landed changes

### 1. A racing merge carries its slot moment; the head and remediating clocks are the durable slot moment

- WHAT: added `frontend.v1.RosterMergeQueueWaiting.racing`
  (`frontend.v1.RosterMergeQueueWaitingRacing`, `since_ms`), present while a
  merge behind the head is admitted (being worked on, racing the head). Re-
  documented `frontend.v1.RosterMergeQueueHead.head_since_ms` and
  `frontend.v1.RosterMergeQueueRemediating.since_ms` as the queue entry's
  durable slot moment.
- WHY: the owner wants every active merge's clock to read how long it has
  held its current slot. A racing member was drawn with its enqueue clock,
  which reads hours for a merge actively being tested.
- Shape: a presence message rather than a bare optional scalar, so the racing
  row element can gain props of its own. `queued_at_ms` stays: it remains the
  waiting clock and a true fact for a racing merge too.
- Consequences: the webapp draws a racing row's clock from `racing.since_ms`;
  the Emacs decoder accepts the new key. The slot moment is written by the
  wsm store in the same transaction as every change to a queue's shape
  (`merge_queue.slot`, `merge_queue.slot_since`), so restarts and handovers
  read it unchanged.
- Sweep: nothing obviated on the wire.
