# Merge queue clocks: active time and restored waiting time

The sidebar's merge queue clocks read the wrong instants. The head's clock ran
from when the merge reached the front of the line, so a head that sat waiting
on a deploy check before it was admitted showed that wait as part of its run
(`keepalive-warmth-cap`, drawn "rebasing / queue / tests" at 46m 43s). A merge
that went back to waiting showed either its whole time since joining the
line, its active stretch included, or a clock restarted from zero.

The owner's rule (2026-11-16): a merge is ACTIVE while it resolves conflicts,
rebases, runs tests, or fixes tests. While active, its clock counts from 0 at
the moment it became active. While waiting, its clock shows how long it has
waited. When an active merge goes back to waiting, its clock takes back its
waiting duration and counts on; the active stretch never counts as waiting.

Design mode: autonomous.

## Assumptions (step 1)

- ASSUMPTION: the involved systems are the daemon (store `wsm`, merge
  orchestrator's roster line, sidebar resolver), the webapp sidebar, and the
  editor's roster decoder (`lisp/wire-roster.el`). Evidence: code (grep of
  every reader of `head_since_ms`, `queued_at_ms` and `since_ms`).
- ASSUMPTION: "active" maps onto the store's standing as: the admitted head,
  the merge holding the fixing-tests slot, and every merge resolving
  conflicts. "Waiting" is a merge in line behind the head, the head before
  admission, and a merge awaiting the fixing-tests slot. A waiting member's
  background rebase is not durable standing (it holds no lease and no
  progress record) and is not drawn on its row, so it does not start an
  active stretch; a background rebase that conflicts steps the merge aside to
  resolving conflicts, which is active. Evidence: code (`wsm.MergeSlot`,
  `wsm.MergeAside`).
- ASSUMPTION: a move between two kinds of active work (the head stepping
  aside to fix its tests or resolve its conflicts) continues the active
  stretch rather than restarting it, because the merge never stopped being
  active under the owner's rule.

## Iteration sequence

One increment: the clock elements of the merge queue section's lines
(`frontend.v1.RosterMergeQueueHead`, `frontend.v1.RosterMergeQueueWaiting`,
and the three repair lines).

## Landed changes

### 1. Typed clocks on every merge queue line

WHAT: three clock messages were added, `frontend.v1.RosterMergeQueueActiveClock`
(the instant the current active stretch began),
`frontend.v1.RosterMergeQueueWaitingClock` (the instant the current waiting
stretch began, and the waiting summed before it), and
`frontend.v1.RosterMergeQueueHeadClock` (a oneof of the two). Each line
carries the clock its standing allows: the head a head clock (either arm),
a waiting merge and a merge awaiting the fixing-tests slot a waiting clock,
the merge fixing tests and every merge resolving conflicts an active clock.
Retired: `RosterMergeQueueHead.head_since_ms`,
`RosterMergeQueueHeadWaiting.queued_at_ms`, `RosterMergeQueueWaiting.queued_at_ms`,
and `since_ms` on `RosterMergeQueueFixingTests`,
`RosterMergeQueueResolvingConflicts` and `RosterMergeQueueAwaitingFixingTests`.

WHY: the owner's rule above. The daemon states the facts (the stretch's start
and the waiting before it) and the page only counts on, so a deploy cannot
reset or inflate a clock.

CONSEQUENCES:
- A line whose standing allows only one kind of clock carries that kind's
  message directly, so a waiting row with an active clock is unrepresentable.
  Only the head can be either.
- The head's clock follows the merge's activity, not `standing` alone: a head
  a restart returned to its place before it took a step stands `waiting` (not
  admitted) while its clock stays active.
- The clock messages are shared across the lines rather than wrapped once per
  line. Ruling: this follows the package's own precedent
  (`frontend.v1.RosterMergeQueueSubject` is shared across the same lines), and
  every line draws the clock identically in one place (`drawClock`); a
  per-line wrapper would be five identical messages with no line-specific
  prop to carry.
- `RosterMergeQueueHeadWaiting` is now empty: its one field was obviated by
  the head's clock. The arm stays, because it is the head's standing.
- Non-obvious implementation consequences: the store keeps the clock
  durably on each `merge_queue` row (layout 180), maintained in the same
  transaction as the slot restamp (`wsm.restampMergeSlots`), which every
  queue-shape write runs; `wsm.ReturnAdmittedMerge` and
  `wsm.RequeuePausedMerge` hold the active stretch across the restart they
  serve. Rows queued before layout 180 take a clock from what the file
  knows: an active row's stretch from its slot moment, a waiting row's from
  when it was put in line, with no waiting before it.

## Sweep

Package-wide: nothing else references the retired fields; no other
declaration was obviated.
