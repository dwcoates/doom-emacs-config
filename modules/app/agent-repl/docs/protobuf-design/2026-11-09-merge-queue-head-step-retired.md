# The merge queue's head draws no status line (2026-11-09)

The owner asked (2026-11-09) that the sidebar's Merge Queue stop drawing a
status line under the running merge ("testing", "rebasing 3/7", ...): the
head's stage strip (queue / rebasing / tests ...) already says which step it
stands on. A merge remediating out of line draws its reason on its name's own
line as a grey parenthetical, "fixing tests" or "resolving conflicts".

Design mode: autonomous (the change was handed over, not co-designed).

## Assumptions

- Objective: the head's card is its name line and its stage strip, nothing
  more; the remediating card is one line.
- Systems: the roster resolver (`daemon/internal/resolve/sidebar`, producer),
  the webapp sidebar's merge-queue view and the Emacs roster decoder
  (`lisp/wire-roster.el`), both consumers.
- Non-additive changes: one field retired. The remediating reason's words stay
  in the webapp (`remediatingDetail`), the single place they are spelled; the
  wire carries only the reason's arm.

## Iteration sequence

One increment: `frontend.v1.RosterMergeQueueHeadMerging.step`.

## Landed changes

### `frontend.v1.RosterMergeQueueHeadMerging.step` is retired

- What: tag 1 and the name `step` are reserved. The message keeps
  `suspended`, which the stylesheet still reads to still the live stage.
- Why: nothing draws the step any more, and a field drawn nowhere is a dead
  contract the daemon would keep composing for no reader.
- Consequences: the daemon's `mergeStepWords` (the words "rebasing 3/7",
  "fixing attempt 2/3") had the sidebar as its only caller and is deleted;
  the footer keeps its own step words. The Emacs decoder stops accepting a
  `step` key. The head's `waiting` standing also loses its drawn word ("next",
  "paused", "stopped"): the pause is drawn on the group's label and the
  stopped clock, and a waiting head shows no stages.
- `frontend.v1.RosterMergeQueueRemediating.reason`'s comment now states the
  parenthetical drawing, and that an arm this revision does not name draws no
  parenthetical (it was a bare "remediating", a word the owner retired).

## Obviated-declaration sweep

- Nothing else is obviated by this change. `frontend.v1.RosterMergeQueueHeadWaiting.queued_at_ms`
  was already read by no consumer before this change; it is recorded in
  `org/todo.md` rather than retired here.
