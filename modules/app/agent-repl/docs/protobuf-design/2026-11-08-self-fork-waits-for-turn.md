# A self-fork in the first turn waits for that turn (2026-11-08)

The Slack emoji-ack dispatch now runs an intake workspace that reads the
thread, chooses the task's name and prescription, and forks ITSELF into the
workspace that does the work, so the work starts with the intake's reading
of the thread. The intake decides in its conversation's first turn, and a
fork carries only finished turns, so that fork used to be refused
(`fork_source_has_no_finished_turn`).

## Landed decisions

- A create whose requesting WORKSPACE forks its own conversation from inside
  its first turn is HELD, not refused, until that turn ends; the fork then
  carries the turn.
  - `agentrepl.v1.CreateWorkspaceResponse.waiting_for_requester_turn` and
    `agentrepl.v1.WorkspaceCreateProgress.waiting_for_requester_turn`
    (`agentrepl.v1.CreateWorkspaceWaitingForRequesterTurn`) answer it.
  - A fork of ANOTHER workspace in its first turn is still refused.
- The hold is the pending-create set's, beside the network hold: one durable
  row, one join rule, one dequeued notice to the requester (skipped once the
  requester has closed).
- It resumes on a LATCH (`daemon/internal/turnlatch`), released by the
  requester's turn-end and close edges (`wsedges`). The latch is taken before
  the turn is looked at again, so a turn that ends in between is never
  missed. A resumed attempt that meets a still-running first turn is held
  again.
- A held row a successor daemon takes up resumes as the network hold does:
  with the network up its latch is already closed, so it runs at once and is
  held for the turn again if the turn is still running.
- The fork is decided before a supplied name is claimed, so a named self-fork
  builds nothing while it waits.
