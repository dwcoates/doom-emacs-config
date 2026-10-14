# The merge queue's test turn, drawn on the run's tab

The two merges at the front of a repository's queue used to run their full
test gates at the same time, doubling the machine's load, and the run behind
was thrown away whenever the head landed. The owner agreed (2026-10-14) that
only one merge of a repository runs its gate at a time: a per-repository TEST
TURN, granted first come first served from a line of the merges ready to run
their gate. A merge waiting for the turn must say so on its tests tab, so the
wait is never invisible. The owner delegated the contract decision.

## Landed changes

### `frontend.v1.FeedMergeTabTestsRun.state` gains `waiting_for_turn`

- **What.** A third arm, `frontend.v1.FeedMergeTabTestsRunWaitingForTurn`,
  on the run subtab's state: the run's round has opened but its suites have not
  started, because another merge of the repository holds the turn. It carries
  the round's start and `frontend.v1.FeedMergeTestTurnHolder`, the holding
  merge's requesting workspace's name.
- **Why.** The owner asked that the wait be visible on the tests tab. The
  run subtab is the element that stands for one run of the suites, and the wait
  is a state of that run (before it is live), so it is an arm of that run's
  state oneof rather than a second field beside it.
- **Why not the parent tests tab.** `frontend.v1.FeedMergeTabTests` stays live
  while its run waits: the gate as a whole is under way, and the run subtab
  says why nothing is running. Adding the arm to the parent too would draw one
  fact in two places with no new information.
- **Why not the shared badge leaves.** The wait carries a holder, which no
  other tab kind has, so it is a dedicated arm message, not a new shared leaf.
- **Why not the footer.** The footer's testing step line
  (`frontend.v1.FooterMergeStepSuite`) names one suite's edge; the owner asked
  for the tab and the logs. The footer shows the testing step with no line
  while the run waits.
- **Consequences.**
  - The run's drawn duration includes its wait: the live arm carries the same
    `started_at_ms` as the waiting arm, because both are the round's start.
    The daemon's INFO records (join, grant, give-back) carry the exact wait.
  - The webapp's tab strip reads the arm generically (`startedAtMs`), gains a
    glyph for it, counts it as running for auto-selection, and draws a one-line
    account under the strip (`webapp/src/feed/merge/tab-set.ts` `drawSummary`).
  - `holder` is republished whenever the turn passes to a merge other than the
    waiting one; with the production window of two there is at most one waiter.
