# The deploy checks know only the deploy mediator

The user ruled (2026-11-17) that the daemon should know only the one deploy
mediator per repository: no intake, remediation or escalation workspaces,
no ownership of problems by workspace, and no routing between workspaces.
A check holds a stage, never workspace identities. The contract's status
and roster messages named the intake, remediation and escalation
workspaces per stage; they now name the mediator once.

Design mode: autonomous. Iteration sequence: the status message family,
then the roster's comments.

## Landed changes

### agentrepl.v1 merge queue status: one deploy mediator

- WHAT: `MergeQueueStatusAdvanceMediating` gains `deploy_mediator` (tag 8,
  optional). The four stage messages lose their workspace fields (rolling
  out's and assessing's intake, fixing's remediation and intake,
  escalated's escalation), each tag and name reserved; the stage messages
  stay as the arms, now empty.
- WHY: one workspace does every stage, so naming it per stage repeated one
  fact four times and implied separate workspaces.
- ASSUMPTION: the mediator is unset only while a repository's first check
  rolls out, before the mediator is made. Every later stage is reached
  only through the mediator.
- CONSEQUENCES: `daemon/internal/server/mergequeue.go` fills the one field
  from `merge.OwedMediation.Mediator`; `mergequeue_test.go` reads it
  instead of the per-stage refs. The CLI prints the status JSON, so its
  readers see `deploy_mediator` in place of the stage refs.

### frontend.v1 roster label: comments only

- WHAT: the `RosterMergeQueueMediation` arm comments name the mediator as
  every arm's target. No shape changed: each arm still carries its own
  target, resolved by the daemon (figma→idl, one element per label state).
- WHY: the targets were documented as the intake, the remediation and the
  escalation workspace, which no longer exist.

### frontend.v1 footer: the mediator's finish is a merging step

- WHAT: `FooterStatusMerging` gains `testing_fix` (tag 30) and `deploying_fix`
  (tag 31), empty arms drawn "testing fix" and "deploying fix".
- WHY: a mediator whose finish gates, lands or deploys stands by with its
  session ended, and the footer drew that planned stand-by as the
  `agent_repl_fault · dead` fault (owner ruling, 2026-11-17: "testing fix"
  and "deploying fix").
- CONSEQUENCES: the merge orchestrator surfaces the finish as merge facts
  (`state: merging`, one of the two steps) on the footer and the roster,
  derived from the store's finish record at every step change; the roster
  draws it as its `merging` arm (purple), the merging rung outranking the
  fault rung. A finish sent back to fixing, deployed or ended surfaces no
  merge, and the session's own arms stand again.

### frontend.v1 roster: an async merge names its requester

- WHAT: `RosterMergeQueueModeAsync` gains `requester`
  (`RosterMergeQueueModeAsyncRequester { name }`), drawn after the merge's
  name as a smaller, greyer, italic "(async of <name>)" on the waiting and
  off-queue lines, cut with an ellipsis before the clock, its whole text on
  the line's hover (owner request, 2026-11-18).
- WHY: a click on an async merge's line selects its requester, which read as
  the click doing nothing when the requester was the workspace in view.
