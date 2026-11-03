# A remediating merge carries why it stepped aside (2026-11-03)

The owner asked (2026-11-03) that a merge at the head of its queue whose rebase
hits conflicts step aside exactly as a head whose whole suite failed does today
(owner ruling 2026-10-31), and that `merge-queue status` and the sidebar show a
remediating merge with its reason: tests or conflicts. Until now the contract
could only say "remediating", because there was only one reason.

Design mode: autonomous (the change was handed over, not co-designed).

## Assumptions

- Objective: every head step that waits on an agent turn steps aside the same
  way, and every reader of the queue can tell which repair the merge is making.
- Systems: the daemon's merge orchestrator (producer), its status endpoint
  (`agentrepl.v1` consumer of the orchestrator), the roster resolver and the
  webapp sidebar's merge-queue view (`frontend.v1`), and the Emacs roster
  decoder (`lisp/wire-roster.el`).
- Non-additive changes: none on the wire. Both messages gain one field.

## Iteration sequence

One increment per package, walked `agentrepl.v1` then `frontend.v1`, each a
single message gaining a reason; no endpoint changes.

## Landed changes

### `agentrepl.v1.MergeQueueStatusEntryStageRemediating` gains its reason

- What: the empty arm message gains `reason`, a message whose oneof arms are
  `tests` and `conflicts` (each an empty message).
- Why: the status an agent reads must say whether a remediating merge is
  fixing its tests or resolving its conflicts, because the two return
  differently (a test step-aside returns as its one fixing attempt; a conflict
  step-aside spends no attempt and may step aside again).
- Shape: a oneof, never an enum (state is a oneof of dedicated messages). The
  field is always set by the daemon; an unassigned oneof is a reason a newer
  daemon added, so no `unknown` arm.

### `frontend.v1.RosterMergeQueueRemediating` gains its reason

- What: the card gains `reason`, its own `RosterMergeQueueRemediatingReason`
  oneof of `tests` and `conflicts`, duplicated rather than shared with the
  status endpoint's (figma→idl: each surface wraps the fact in its own
  element message).
- Why: the sidebar card's line beneath the name says what the merge repairs.
- Consequence: the webapp draws "remediating tests" / "resolving conflicts"
  from the arm, and a bare "remediating" for an arm it does not know; the
  Emacs decoder decodes the arm as a keyword.

## Where the reason comes from (implementation note)

- The reason is NOT a new store column. It is read off the merge's durable
  progress record, whose step names the turn the merge waits on: `fixes` is
  tests, `conflicts` is conflicts. The record is checkpointed before the
  prompt is submitted, so it already names the turn when the entry leaves the
  line, and it survives a restart. A store column would be a second copy of
  the same fact that could disagree with the record.
- A remediating entry whose record is missing or names any other step is an
  invariant violation: the snapshot fails loudly rather than guessing.
