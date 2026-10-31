# A final response being updated reads `updating`, red

When words of a settled final response are highlighted and a prompt is sent,
the daemon rewrites that response on a side query instead of running a turn
(`daemon/internal/revision`). The owner asked (2026-10-30) that while this
runs the workspace's status turns to the working red, and the footer's status
word reads "updating" instead of "working".

## Assumptions (autonomous design)

- Objective: the status surfaces say a response update is in flight, in the
  working register, with its own word.
- Systems: daemon (footer resolver, roster projection, workspace status,
  reviser), webapp (footer strip). The Emacs tab bar paints the roster arm, so
  it needs nothing.
- No non-additive change.

## Landed changes

### `frontend.v1.FooterStatus.updating` (tag 21)

- WHAT: a new status arm, `FooterStatusUpdating`, carrying only its activity
  cell (`FooterStatusUpdatingActivity`: the shared salient kinds, or unpinned),
  shaped exactly like `loading`'s cell.
- WHY: the footer's status word is the arm's name, so "updating" in place of
  "working" is a status arm of its own rather than a `working` substatus
  (which would read "working · updating") or a client-side rewording (which
  the footer contract forbids: a client words nothing).
- No substatus oneof: the bubble being updated is what says which response.
- Consequences:
  - It projects onto the ladder's THINKING rung (`ladder.FooterClaim`), so the
    roster draws `thinking` and every surface is red. No roster arm is added:
    the roster's red arm already exists and the owner asked only for the color.
  - Within the thinking rung it ranks BELOW the turn: a turn in flight draws
    `working`. Rationale: a turn is the bigger fact, and a revision running
    beside one is rare.
  - The fact is the reviser's lane (`revision.Reviser`): raised when a lane
    starts, lowered when it drains, so several queued revisions read as one
    continuous `updating`.
