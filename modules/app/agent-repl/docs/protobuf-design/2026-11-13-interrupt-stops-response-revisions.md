# A turn stop stops the workspace's response revisions

The stop key (`C-c C-k` in the composer, and the footer's stop) could not stop
a response revision: highlighting words of a final response and sending a
prompt rewrites that response on a shim side query, and nothing the user could
press ended it. The owner asked that interrupts interrupt response updating.

## Assumptions (autonomous design)

- Objective: the turn stop also stops response revisions.
- Systems: daemon (`internal/revision`, `internal/server` Interrupt), shim
  (`engine/revise-response.ts`, honoring the abort it already receives),
  Emacs (`wire-verbs.el`, `verbs.el`), webapp (footer and feed stop notes).
- No non-additive change: one new `InterruptSuccess` arm.

## Iteration sequence

One increment: the outcome arm on `agentrepl.v1.InterruptSuccess`, and the
precedence stated on `agentrepl.v1.InterruptTurn`. Protocol, inventory and
conventions were unchanged, so the earlier stages were already settled.

## Landed changes

### `interrupted_revisions` outcome, revisions stopped first

- WHAT: `agentrepl.v1.InterruptSuccess` gains the arm
  `interrupted_revisions` (`agentrepl.v1.InterruptedRevisions`, a count), and
  `agentrepl.v1.InterruptTurn.roll_back_unanswered`'s comment states that a
  turn stop with revisions running stops those revisions instead of anything
  else.
- WHY: the stop key must reach the rewrite the owner is watching.
- WHY REVISIONS FIRST, NOT BOTH: a revision runs beside a turn in flight, so a
  user who stops a rewrite they just asked for should not lose a long running
  turn with it. This mirrors the held-prompt rule already on `InterruptTurn`
  (the newest thing the user asked for is taken back first, the turn runs on,
  and the next press stops the turn). Stopping both at once was rejected
  because the answer would then need to carry two outcomes, and the second
  press already reaches the turn.
- WHY NOT `nothing_running`: that arm is the second press that offers a
  rollback, so answering it after stopping a revision would roll back a
  prompt the user never meant to touch.
- CONSEQUENCES:
  - A stopped revision clears its response's revision state (no pending, no
    failed); the response stands as it was.
  - A revision the shim had already written before the cancel reached it
    still lands; the stop can only be too late, never half-applied.
  - The webapp's footer and feed stop notes and Emacs's interrupt verb each
    gain a case for the arm.

Sweep: nothing obviated.
