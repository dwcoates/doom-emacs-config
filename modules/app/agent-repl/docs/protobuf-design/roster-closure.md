# Roster row: when the workspace was closed, and by whom

The owner asked for `SPC p p` to offer, beside the open and hidden
workspaces and the 25 most recently merged, the 25 workspaces the USER most
recently closed or killed: explicitly, by their own gesture, never one the
daemon or an agent closed on its own. The roster said only THAT a row was
closed (`RosterRowClosed`), never when or why, and the daemon's record held
nothing more (`wsm.Workspace.Closed`). An editor-local record of its own
close gestures would forget every close made from another client (the
sidebar's close button, a second Emacs) and would be empty after every Emacs
restart.

## Landed changes

### `frontend.v1.RosterRow.closure` (field 66)

- WHAT: a new optional element on the roster row, `RosterRowClosure`,
  carrying `at_ms`, the close instant, and the `by` oneof naming who closed
  it: `user` (CloseWorkspace or KillWorkspace), `agent` (a workspace command
  file), `merge` (the merge that landed it), or `daemon` (unasked: the
  workspace's directory was gone). Like `last_selected` and
  `last_turn_ended` it is for ordering and filtering only, never drawn.
- WHY: the switcher needs "closed by the user, most recent first" to survive
  restarts of every client, and the daemon is the one place every close goes
  through.
- THE FIRST CLOSE IS THE ONE RECORDED. A workspace closed twice (a kill marks
  the row closed before its teardown and again after it, a boot re-closes a
  row to release its serving ownership) keeps its first instant and cause,
  except that a user close takes over a close the user did not make: the
  user killing a workspace the daemon had closed is the user's kill. A
  re-open clears the record.
- Consequences:
  - WSM gains `workspaces.closed_at` and `workspaces.closed_by` (layout 52),
    written in the same statement that sets `closed`. A row already closed
    when the columns land has neither: the daemon cannot vouch for when, or
    by whom, so the field is unset for it rather than guessed.
  - Every close in the daemon names its cause; `CloseWorkspace` and
    `KillWorkspace` are the user's, a command file's close is an agent's.
  - The daemon's row resolver (`daemon/internal/resolve/sidebar/rows.go`)
    sets it from `rec.Closure`.
  - The editor's roster decode (`lisp/wire-roster.el`) carries it, and the
    `SPC p p` switcher filters by it.
  - The webapp ignores it; nothing draws it.
