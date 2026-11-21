# Merge-queue move up and down: a move that moves nothing is refused

`agent-repld merge-queue move-up` and `move-down` answered success when the
merge did not move (2026-11-20 ~19:56: `move-up wifi-failover-known-join-option`
answered success twice while the merge stayed fourth, behind three merges an
earlier build had returned admitted from their repairs). A move that moves
nothing must answer a typed refusal naming why, and name the entry it cannot
pass.

Design mode: autonomous.

## Assumptions (derived from the request and the code)

- Objective: every one-place move that leaves its merge in place answers a
  typed refusal on `UpdateMergeQueue`, naming its cause and, where one stands
  in the way, the entry.
- Built: one new `UpdateMergeQueueError` arm, the store's move answering its
  outcome instead of a bool, the orchestrator turning a blocked outcome into
  that refusal, the server filling it, the CLI's sentence, and the editor
  lisp's decoder.
- No frontend component: the sidebar's drag sends `move_to`, not one-place
  moves. The webapp only regenerates its bindings.
- Systems: the protobuf contract, the daemon (wsm, merge, server, CLI), the
  editor lisp wire codec.
- Non-additive changes: none on the wire. In the daemon, `wsm.DB.MoveMerge`
  answers `wsm.MergeMoveOutcome` instead of `bool`, and
  `merge.Orchestrator.Move` answers `error` alone.

## Iteration sequence

1. Transport: the existing `UpdateMergeQueue` rpc (a refusal arm, not a new
   rpc).
2. The refusal arm and its cause oneof.

## Landed changes

### `UpdateMergeQueueError.move_blocked` (tag 25) and `UpdateMergeQueueMoveBlocked`

- WHAT: a refusal arm whose `cause` oneof is `not_waiting` (the merge is not
  waiting in line), `at_edge` (no entry in line on that side) or `admitted`
  (every entry in line on that side is admitted; carries the nearest one as
  `UpdateMergeQueueMoveBlocker { workspace, name }`).
- WHY: a move that answered success while moving nothing hid that three
  admitted returners stood in the way.
- Tag 25, not 23: the `merge-queue-move-verb` merge, in line ahead of this
  one, adds tags 23 and 24 to the same oneof. Whichever lands second keeps
  both sets; the numbers do not collide.
- `not_waiting` is its own cause here, while `merge-queue-move-verb`'s `move`
  arm answers a merge that is not waiting with `no_such_queued_merge`. The two
  spell one fact two ways; aligning `move` onto this cause is owed once both
  have landed.
- The blocker names the merge as `merge-queue status` lists it (its
  workspace's name), and carries the `WorkspaceRef` it runs in.

### Consequences

- The server's refusal filler (`internal/server/refuse.go`, `setNestedArms`)
  now fills a nested cause arm's own fields from the refusal's fields, so a
  cause can carry evidence (`admitted.blocker`). Before, a nested arm was
  always set empty.
- A move that moved nothing no longer emits a queue change or republishes the
  roster, as before; it now also reaches its caller as an error arm.
- The refusal is recorded once, at INFO, on `daemon.merge.move`, with the
  merge, the move, the result and the blocker.

## Sweep

No declaration is obviated.
