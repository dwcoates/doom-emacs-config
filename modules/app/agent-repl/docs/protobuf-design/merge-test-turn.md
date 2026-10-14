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

## Implementation consequences (daemon, `internal/merge`)

- **The turn and its line live in `testturn.go`**, under the orchestrator's
  lock; `gate` (`gate.go`) takes the turn after selecting suites (an empty
  selection runs nothing and takes no turn) and gives it back on every way
  out. `releaseSlot` gives back what a run leaving its chain still holds.
- **The governor's permit is taken after the turn**, inside `gate`
  (`admitGate`), for the head and the merge behind it alike. A merge behind
  the head that holds the turn unadmitted gives it to its head when the head
  joins the line (`errTurnToHead`, `watchHeadInLine`), and rejoins the line
  behind it: the governor's head-first rule holds through the turn. Without
  that, a busy machine would have held the head's gate behind a merge that
  runs nothing.
- **A merge promoted before the governor admitted it** settles its round
  ("the merge reached its queue's head") and runs its gate from the head's own
  steps, which rebase first. Running it at once as the head's gate would have
  tested a base the landing that promoted it had just moved.
- **Accepted cost: a brief run on an old tip.** When the head's gate passes it
  gives back the turn at once, so the merge behind may start a run that the
  head's landing cuts moments later. The owner's design releases the turn
  whenever a run ends, and the merge behind may still win the race to land
  while the head is in its post-merge steps, so the run is not withheld.
- **A run in line stands at a step boundary** (`run.idling`), so a
  workspace restart suspends it at once instead of waiting through another
  merge's whole run.
- **Tests that needed two gates at once were re-arranged, not weakened**:
  the chain and race suites order the two merges with the TEST-ONLY seams
  `onTestTurnAsk` / `onTestTurnLine` (`chain.parkTurn`, `chain.awaitLined`),
  and `chainRunner.overlapped` asserts no two gates ever ran together.
