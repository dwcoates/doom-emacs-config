# Merge queue: three slots, no racing (2026-11-11)

The owner ruled that a merge queue holds at most three active merges, one per
slot: one merge runs the test gate (the head), at most one other fixes its
tests out of line, and at most one other resolves conflicts out of line.
Racing (two merges testing at once on AC power) is removed entirely, and the
sidebar never draws "front". The contract has to say this: the roster's list of
remediating merges had no cap (four sat in conflict resolution at once on
2026-11-10), and racing left switches, arms and fields across three packages.

Designed autonomously (no co-design was asked for).

## Assumptions

- ASSUMPTION: the objective is the owner's ruling as stated in the task: three
  slots per queue, a merge needing a taken slot waits for it in order, racing
  and the "front" label go. Evidence tier: the request itself.
- ASSUMPTION: systems involved are the daemon (`merge`, `wsm`, `cmd`), the
  webapp sidebar and feed, and the editor lisp wire codecs, all within
  `modules/app/agent-repl/`. Evidence: grep of every racing/remediating/front
  symbol across the module.
- ASSUMPTION: the frontend components affected are the sidebar's merge-queue
  group (figma→idl governs) and the feed's tests run subtab.

## Iteration sequence

Default sequence applied: the endpoint inventory is unchanged; walked
`agentrepl.v1` (UpdateMergeQueue, MergeQueueStatus), then `frontend.v1`
(sidebar roster queue, feed tests run), then the one `repoconfig.v1` comment.

## Landed changes

### UpdateMergeQueue loses its racing switch

- WHAT: `UpdateMergeQueueRequest.disable_racing` (9) and `.enable_racing` (10)
  are reserved, and `UpdateMergeQueueError.racing_already_disabled` (11) and
  `.racing_not_disabled` (12) are reserved. Their four messages are deleted.
- WHY: no racing exists to switch; the owner asked that no dormant flag stay.
- CONSEQUENCES: the `merge-queue enable-racing|disable-racing` verbs, the
  daemon's admin arms (`daemon/internal/server/admin.go`), the elisp encoders
  and decoders in `lisp/wire-verbs.el`, and the store's
  `merge_racing_disabled` table (dropped by layout 123) all go.

### MergeQueueStatus names each slot

- WHAT: `MergeQueueStatusEntryStage.remediating` (3) and its reason messages
  are retired. The stage gains `fixing_tests` (4), `resolving_conflicts` (5)
  and `awaiting_repair` (6), the last carrying which slot it awaits (a oneof
  reusing the two holder messages as its arms) and its 1-based place among the
  merges awaiting that slot. The queue's comment states the three-slot rule.
  `MergeQueueStatusEntry.placed_at_front` is kept: the task removes the label
  from the sidebar, and the agent-facing status still reports placement.
- WHY: `merge-queue status` must show at most one merge per slot and who waits
  for which slot. The entry list keeps every merge of the queue in one order
  (line, then holders, then awaiting) so notices and reads still share one
  shape.
- CONSEQUENCES: the daemon's status builder and the merge-queue notices
  (`daemon/internal/mergenotice`) switch on the new arms; the
  `/agent-repl-merge-queue` skill's description of status changes.
- WHY NOT queue-level optional holders here: status is one ordered entry list
  shared by reads, subscriptions and notices; the per-entry arm keeps that,
  while the store's one-holder index is what guarantees at most one holder.

### The roster's queue holds single repair-slot holders

- WHAT: `RosterRepoMergeQueue.remediating` (5) is retired with
  `RosterMergeQueueRemediating` and its reason messages. The queue gains
  `optional RosterMergeQueueFixingTests fixing_tests = 7`,
  `optional RosterMergeQueueResolvingConflicts resolving_conflicts = 8`,
  `repeated RosterMergeQueueAwaitingFixingTests awaiting_fixing_tests = 9` and
  `repeated RosterMergeQueueAwaitingResolvingConflicts awaiting_resolving_conflicts = 10`.
  Each carries the subject and its durable slot moment (`since_ms`).
- WHY: the owner asked that the roster shape say "at most one merge per slot":
  a single optional holder makes a second holder unrepresentable on the wire.
  Under figma→idl each drawn row role is its own message, so the two holders
  and the two awaiting rows are four messages, not one with a reason oneof.
- Layout (figma→idl):

```text
+-RosterRepoMergeQueue------------------------+
| RosterMergeQueueHead (testing slot)         |
| RosterMergeQueueFixingTests   "fixing tests"|
| RosterMergeQueueResolvingConflicts          |
|                        "resolving conflicts"|
| RosterMergeQueueAwaitingFixingTests ...     |
| RosterMergeQueueAwaitingResolvingConflicts  |
| RosterMergeQueueWaiting ...                 |
+---------------------------------------------+
```

- CONSEQUENCES: `daemon/internal/merge/rosterqueue.go` fills the four fields
  from the store's stamped slots; `webapp/src/sidebar/merge-queue-view.ts`
  draws them; `lisp/wire-roster.el` decodes them.

### The waiting row loses racing and "front"

- WHAT: `RosterMergeQueueWaiting.placed_at_front` (3) and `.racing` (4) are
  reserved; `RosterMergeQueueWaitingRacing` is deleted.
- WHY: the sidebar draws no "front" (owner ruling), and nothing else read the
  roster's field (the elisp decoder decoded it and drew nothing from it);
  racing is gone. Ordering by `-placement front` is the store's
  `source_front`, untouched.

### The feed's tests run never waits for a test turn

- WHAT: `FeedMergeTabTestsRun.waiting_for_turn` (4) is reserved;
  `FeedMergeTabTestsRunWaitingForTurn` and `FeedMergeTestTurnHolder` are
  deleted.
- WHY: only the head runs the gate (structurally: the gate asserts its run
  holds the testing slot), so no run ever waits behind another's tests; the
  daemon's test-turn line (`testturn.go`) is deleted with it.

### Comment-only changes

- `repoconfig.v1.DeployLocalMerge` no longer says deploys behind the head test
  ahead of their turn. Sidebar comments name the repair slots instead of
  remediating merges.

## Obviated-declaration sweep

Package-wide reference graph read after the landings: no remaining reference
to the deleted messages in `proto/src`. Nothing else found.

## Retired after the rebase onto master (2026-11-12)

Master landed a second design for a merge back from a repair out of line
(`frontend.v1.RosterMergeQueueWaiting.placement`, its
`frontend.v1.RosterMergeQueueWaitingPlacement` oneof of `front` and
`returned`, and the `frontend.v1.FeedMergeTabHeld` state of the repair's
tabs). Under three slots a merge back from a repair is a plain waiting merge
drawn by its slot, so all of it is retired:

- `frontend.v1.RosterMergeQueueWaiting`: tag 5 (`placement`) is reserved with
  tags 3 and 4.
- `frontend.v1.RosterMergeQueueWaitingPlacement`,
  `frontend.v1.RosterMergeQueueWaitingFront` and
  `frontend.v1.RosterMergeQueueWaitingReturned` are deleted.
- `frontend.v1.FeedMergeTabHeld` is deleted, and the `held` arm of each tab
  state oneof is reserved by number and name in `frontend.v1.FeedMergeTabRebasing`,
  `FeedMergeTabRebaseCommit`, `FeedMergeTabResolvingConflicts`,
  `FeedMergeTabConflictResolution`, `FeedMergeTabTests`,
  `FeedMergeTabFixingTests` and `FeedMergeTabTestsRunFixing`.
