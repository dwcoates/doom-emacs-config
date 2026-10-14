# Roster row: when the workspace's last turn ended

The owner asked for `SPC TAB TAB` to switch to the workspace that most recently
finished: the one that most recently went from working (red) to done (green).
The editor can only answer "most recently" from an instant, and the only
instant it had per row was the durable selection instant, which times viewing,
not finishing. An editor-local record of its own finish edges would forget
every finish it did not watch (one that landed while Emacs was down or the
roster stream was reconnecting) and would be empty after every Emacs restart.
The daemon already keeps the durable instant, `wsm.Workspace.LastTurnEndedAt`,
read off the turns' `closed_at`.

## Landed changes

### `frontend.v1.RosterRow.last_turn_ended` (field 65)

- WHAT: a new optional element on the roster row, `RosterRowLastTurnEnded`,
  carrying `at_ms`, the close instant of the workspace's most recent turn, of
  any close kind. It mirrors `RosterRow.last_selected` in shape and role:
  ordering only, never drawn.
- WHY: the editor's `SPC TAB TAB` needs finish recency that survives restarts
  of every client, and the daemon is the one place that holds it.
- How a client uses it: pair it with the row's status arm. The instant says
  when the last turn ended; the arm says how the workspace stands now. The
  editor's jump takes rows whose arm is `done` and picks the latest instant.
  A workspace whose next turn already runs is excluded by its arm.
- Consequences:
  - The daemon's row resolver (`daemon/internal/resolve/sidebar/rows.go`)
    sets it from `rec.LastTurnEndedAt`, beside `lastSelected`.
  - The editor's roster decode (`lisp/wire-roster.el`) carries it.
  - The webapp ignores it; nothing draws it.
- The contract deliberately does NOT promise that a turn-end arm implies the
  field is set. The arm comes from the daemon's live close state
  (`wsState.turnEndArm`, including `restoredEnd` from `result_end`), while the
  instant comes from the turns table. No evidence was gathered that every
  `result_end` has a closed turn row behind it (a forked workspace is the
  suspected case), so a consumer treats an unset field as "nothing to rank
  by" rather than as a breach.
