# A landed merge shows as deploying until its deploy check ends

The owner wants a merge's deploying stage to run from the moment its deploy
starts until the deploy mediator decides on that deploy (approve, escalate,
dispatch, or a release without the mediator's word). Before this change the
merge's run left the queue as soon as its deploy hook asked for the rollout
(under a second), and the check that followed was drawn only as a label beside
the repository's name.

## Assumptions (autonomous design)

- No new field or message is needed. Every surface already has a place for a
  merge being worked on: the status's `running` entry, the roster's head, and
  the queue tab's `merging` entry ahead. The landed merge is drawn there.
- Queue advancement is unchanged. The queue's head was already held from the
  deploy's start until the mediator's verdict (`merge.deployHold`); this
  change only draws and measures.

## Landed changes

### Comment-only clarification of three surfaces

- WHAT: the comments on `agentrepl.v1.MergeQueueStatus` (and its `entries`),
  `frontend.v1.RosterRepoMergeQueue.head` and `frontend.v1.FeedMergeQueue.ahead`
  now say that a landed merge whose deploy's check is still owed stands at the
  front, running or merging, until the check ends. No shape changed.
- WHY: the owner's request that the merge queue show one deploying stage from
  deploy start to the mediator's verdict.
- CONSEQUENCES:
  - The deploying merge holds the queue's head from its landing's teardown
    (`merge.deployHold`, a later change on 2026-11-24): the teardown records
    it before it kicks the queue's pump, so no merge is admitted between the
    landing and the deploy's `Building` edge, when the check becomes owed.
    Before that change a merge admitted in that instant ran beside the
    deploying merge and kept the roster's head. The record's stage
    (`wsm.DeployingStage`) ties it to the deploy that carries it: covered at
    that deploy's commit, checked in the write that owes its check, and ended
    in the write that releases the check.
  - The daemon keeps the deploying merge in the store
    (`wsm.DeployingMerge`, layout 205), because the deploy hands the daemon
    over to a successor, which draws the merge and ends it.
