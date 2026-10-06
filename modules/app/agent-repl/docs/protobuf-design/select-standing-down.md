# SelectWorkspace during a daemon's stand-down

A workspace selection that reaches a daemon while it is standing down records
the selection, then cannot revive the workspace's session because the shim
supervisor refuses new spawns. The daemon answered with an arm the contract did
not have (`spawn_failed`), which surfaced as a WARN on the daemon and an ERROR
in Emacs for an ordinary, expected answer.

## Landed changes

### SelectWorkspaceError gains `standing_down` (owner ruling, 2026-10-06)

- WHAT: a new arm `standing_down` (empty message `SelectWorkspaceStandingDown`)
  on `SelectWorkspaceError`.
- WHY: "the daemon is standing down" is a real answer the client can act on by
  re-asserting the selection on the daemon that serves next. None of the
  existing arms fit: `transferring_away` needs a successor's address, which a
  plain stop or restart does not have, and `not_yet_adopted` is a joining
  successor's state.
- The alternative of answering success for this cause was rejected: the
  select contract states that a revival that fails fails the select, and the
  owner kept that rule.
- Consequences: the daemon maps the select's revival `shimclient.ErrStandingDown`
  to the arm at INFO (daemon/ERROR-ARMS.md row); Emacs decodes the arm at INFO
  and re-asserts the selection on the next daemon instead of reporting a
  failure. The selection itself is already durable (SetCurrent precedes the
  revival), so the arm reports only the session not being started.
