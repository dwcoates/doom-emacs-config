# A merge returned to the line is drawn waiting, and is marked returned

A merge that steps aside to repair out of line (fixing its tests, resolving
its conflicts) goes back in line once its repairing turn ends. Until it heads
the queue again nothing of it runs, yet `merge-queue status` listed it running
its recorded step, its bubble kept the repair's tab active with a clock
counting, and the sidebar labelled it `front`, because the return wrote the
same front mark a user's front placement writes (2026-11-10:
`sidebar-extra-repo-blocks`, `topbar-canonical-links-styling`,
`sidebar-fold-workspace-selection`).

Design mode: autonomous (the change was handed over with the proto change
pre-approved).

## Assumptions

- Objective: a returned merge reads as waiting on every surface until it is
  taken up again, and the sidebar tells "returned" apart from "front".
- Frontend present: the sidebar queue's waiting row and the merge bubble's tab
  strip (figma→idl governs both).
- Systems: daemon (wsm store, merge orchestrator, sidebar resolver, server
  status) and webapp (sidebar queue view, merge tab strip). No other
  producer or consumer reads the touched messages (verified by grep for
  `placed_at_front` / `PlacedAtFront` and the touched tab kinds).
- Non-additive: `frontend.v1.RosterMergeQueueWaiting.placed_at_front` is
  retired (tag 3 reserved).

## Iteration sequence

Two increments, each a single UI element: the waiting row's placement label,
then the bubble tab's held state.

## Landed changes

### 1. `frontend.v1.RosterMergeQueueWaiting.placement`

- WHAT: the bool `placed_at_front` becomes `optional
  RosterMergeQueueWaitingPlacement placement`, a oneof of `front` and
  `returned`. Unset is an ordinary placement at the back.
- WHY: the label after a waiting merge's name answers one question, "why does
  it stand ahead of merges queued before it", and the two answers are
  exclusive. A second bool beside the first would make both-set
  representable.
- Consequences:
  - A merge the user put at the front that later stepped aside and came back
    draws `returned`: it is the latest fact and the one that placed it.
  - The store records the two facts separately: `merge_queue.source_front`
    stays the user's placement; the return no longer writes it, and a new
    `merge_queue.returned` column (layout 125) carries the return.
  - `agentrepl.v1.MergeQueueStatusEntry.placed_at_front` is left as is; it
    now reads only the user's placement, because the return no longer writes
    the front mark.

### 2. `frontend.v1.FeedMergeTabHeld`

- WHAT: a shared state leaf, `held`, added as an arm to every tab kind a
  repair out of line leaves open: rebasing, rebase_commit,
  resolving_conflicts, conflict_resolution, tests, fixing_tests,
  tests_run_fixing.
- WHY: after the repairing turn ends, the round is neither running (nothing
  of the merge runs while it waits) nor settled (only the merge, once it
  heads its queue, reads whether the repair worked). Live drew a ticking
  clock and auto-selected the tab; settled would state an outcome nobody has
  read yet.
- Consequences:
  - The daemon draws the held tabs from the merge's progress record when it
    returns (and when a restart takes a returned merge back); the resumed run
    redraws them live through its ordinary resume drawing, so no step of the
    run learns about the held state.
  - The client draws a held tab with no clock and never selects one of its
    own accord: a strip whose latest tab is held shows no tab.
  - The tests run subtab (`tests_run`) and the queue tab get no held arm: the
    run a fixing attempt follows has already settled, and the queue tab
    settled at the merge's first admission.
