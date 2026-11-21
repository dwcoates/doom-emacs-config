# Merge-queue move: a waiting merge put at any place in one call

The owner asked (2026-11-20) for one `agent-repld merge-queue move MERGE|here
-placement front|back|behind=MERGE|index=N` verb. Until now an agent could only
move a waiting merge one place per call (`move-up`, `move-down`), so putting a
merge at the head of a long line took dozens of calls, and the commandeering
procedure in the merge-queue skill looped `move-up`. `requeue -placement`
applies only to a merge off the queue.

Design mode: autonomous.

## Assumptions (step 1, derived from the request and the code)

- Objective: one daemon call that reorders a queue's waiting line atomically,
  putting one waiting merge at front, back, behind a named merge, or a numbered
  place.
- Built: a new `UpdateMergeQueueRequest` arm, its store transaction, the
  orchestrator's reorder, the server arm, the CLI verb, and the skill text.
- No frontend component: the sidebar's drag keeps `move_to`.
- Systems: the protobuf contract, the daemon (wsm store, merge orchestrator,
  server, `agent-repld` CLI). The webapp and the editor lisp send no move of
  this kind; their bindings regenerate only.
- Non-additive changes: none to the wire. The store's existing front move
  (`wsm.PrioritizeMerge`) and the move beside an anchor (`wsm.MoveMergeTo`)
  share the reorder helper the new move adds (a behavior-preserving extraction).

## Iteration sequence

1. Transport: the existing ConnectRPC `UpdateMergeQueue` rpc (an arm, not a new
   rpc, since every other queue reorder is an arm of it).
2. The request arm and its place oneof.
3. The refusal arms.

## Landed changes

### `UpdateMergeQueueRequest.move` (tag 19) and `UpdateMergeQueueMove`

- WHAT: a new action arm carrying the moved merge's workspace and a `place`
  oneof of four arms: `front`, `back`, `behind` (a merge name) and `index`
  (a 0-based place among the waiting merges).
- WHY: the owner wants one call per move, whatever the distance.
- Why its own arm messages rather than `agentrepl.v1.MergeQueuePlacement`:
  that placement's `front` means "behind any earlier front merge", while the
  move's `front` means "ahead of every waiting merge, earlier front placements
  included", and its `behind` sets up a durable wait on the named merge
  landing, which a move must not. Same names, different facts, so they are not
  shared. `index` has no meaning for a merge joining the line.
- `behind` carries the merge's NAME as the queue lists it, not a
  `WorkspaceRef`: the refusal `placement_behind_not_in_line` reports the name
  asked for, and the daemon resolves the name inside the queue it reorders
  (supplied or prefix-resolved, exactly as `MergeQueuePlacementBehind` is
  resolved at request time). Behind the RUNNING merge is allowed and is the
  head of the waiting line.
- Front marks: the moved merge at the head is marked front; anywhere else it
  takes the mark of the waiting merge directly ahead of it. This keeps every
  front merge ahead of every ordinary merge waiting, which the store's other
  reorders also guarantee.
- Consequence: `index=0`, `front` and `behind=RUNNING` are the same place.

### `UpdateMergeQueueError.placement_behind_not_in_line` (23) and `index_out_of_range` (24)

- WHAT: two refusal arms. The first reuses
  `agentrepl.v1.MergeQueuePlacementBehindNotInLine` (the same fact the request
  verbs refuse). The second is `UpdateMergeQueueIndexOutOfRange { index,
  waiting }`.
- The owner's brief named `unknown_merge`. `UpdateMergeQueue` already spells
  "no merge of this workspace to act on" as `no_such_queued_merge`, so the move
  answers that arm rather than adding a second spelling of one fact. A merge
  running, only requested or stepped aside is refused the same way: it is not
  WAITING, and the move never moves it (unlike `move_up`, which answers success
  and leaves it).
- Consequence: the store validates the anchor's and the index's standing inside
  its write transaction, so a queue change between the name's resolution and
  the reorder is refused rather than half-applied.

## Sweep

No declaration is obviated: `move_up`, `move_down` and `move_to` stay (the
sidebar's drag and the one-place verbs still send them).
