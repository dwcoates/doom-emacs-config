# The unread turn end under an idle_async row

The owner wants a workspace in the background (`idle_async`, yellow) whose last
turn's result is still unread to say so: in the sidebar the status dot stays
yellow while the workspace NAME takes the ready green, and in the Emacs tab bar
the `[N]` bracket stays yellow while the workspace name takes the green. Once
the user views the workspace, the whole row and tab go back to yellow.

## Landed changes

### 1. `frontend.v1.RosterRow.idle_async_unread` (tag 69)

- WHAT: a new optional marker `frontend.v1.RosterRow.idle_async_unread`,
  typed `frontend.v1.RosterRowIdleAsyncUnread`, whose `turn_end` oneof names
  the unread turn end by its status arm message (`done`, `interrupted`,
  `turn_failed`, reusing `frontend.v1.RosterRowStatusDone` and its siblings).
  It is present exactly while the row's status is `idle_async` and the
  daemon's read-result fact is unread.
- WHY: the daemon already holds the fact (`wsState.result`, read by
  `SetViewed` on an `idle_async` row through `viewReadsResult`), but no wire
  field carried it, so neither renderer could tell an unread background row
  from a read one. `frontend.v1.RosterRowViewed` cannot carry it: that marker
  is turn-end-only by contract and means the opposite (read).
- WHY THE TURN END IS NAMED: the owner asked for "the green idle/ready color",
  which is what `done` and `interrupted` draw. A failed turn's result under a
  background row would be drawn green, a claim that the turn succeeded, so the
  marker names the turn end and each renderer inks the name with that arm's
  shared color (`render-colors.json#roster_status`). `turn_failed` therefore
  draws a turquoise name. No new vocabulary is needed.
- WHY NOT A STATUS ARM: the workspace is in the background, and every rule
  keyed on the `idle_async` arm (the dot, the dwell, `viewReadsResult`) must
  keep holding. This follows the async-merge marker's precedent
  (`async-merge-row-marker.md`).
- CONSEQUENCES:
  - `daemon/internal/resolve/sidebar/rows.go` sets the marker in the same
    render as the status, from `s.resultUnreadNow()` and `s.turnEndArm()`.
  - The webapp sidebar (`webapp/src/sidebar/row.ts`) inks the name with the
    turn end's tone class while the row keeps the `idle_async` tone on its dot.
  - The Emacs tab bar (`lisp/status.el`) paints the name half of the tab with
    the turn end's palette colors while `[N]` keeps the yellow. A tab already
    drawn bracket-only (panels closed) has no colored name half, so the marker
    changes nothing there.
  - The elisp wire codec (`lisp/wire-roster.el`) must decode the field,
    because it refuses unknown keys.
