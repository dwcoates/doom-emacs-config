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
  - The status's old claim of "at most one entry is running" no longer holds:
    a merge admitted in the instant between the landing's teardown and the
    deploy's `Building` edge (when the check becomes owed) runs beside the
    deploying merge. The comment states it. The admission race itself is
    unchanged by this work, since the task forbade changing advancement.
  - In the roster, a front already admitted keeps the head in that same
    instant, and the deploying merge is then not drawn there (it still is in
    the status and the queue tabs). Accepted as a cost of having one head.
  - The daemon keeps the deploying merge in the store
    (`wsm.DeployingMerge`, layout 205), because the deploy hands the daemon
    over to a successor, which draws the merge and ends it.
