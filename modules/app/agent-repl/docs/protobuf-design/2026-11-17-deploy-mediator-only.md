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
