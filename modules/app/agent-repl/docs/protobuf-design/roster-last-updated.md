# Roster row: when the workspace was last updated

The owner asked for `SPC p p` to offer workspaces most recently updated first
(2026-10-14): the ones whose turn most recently resolved, the ones most
recently opened, closed or killed, and the ones whose status most recently
changed. The roster rows carried several instants (`last_selected`,
`last_turn_ended`, `closure`, the when-column's merge instant) but none of
them covers all of that, and selection is viewing, not updating. Nothing at
all recorded a status change or a re-open. So the daemon resolves one instant
for every row, and the editor only orders by it.

## Landed changes

### `frontend.v1.RosterRow.last_updated` (field 67)

- WHAT: a new REQUIRED element on the roster row, `RosterRowLastUpdated`,
  carrying `at_ms`: the latest of the workspace's creation, its latest turn
  start or close, its close record, its hidden instant, its merge landing, a
  requester's landing, and its last status change. Ordering only, never
  drawn.
- WHY REQUIRED: every workspace has at least its creation instant, so there is
  no row with nothing to state. The editor's decoder refuses a row without it.
- WHY THE DAEMON RESOLVES IT: every client ranks by the same value, and the
  status change instant only exists in the daemon.
- Selection does NOT move it: viewing is not an update, and selection recency
  is `RosterRow.last_selected`.

### The status change instant (`wsm` layout 54)

- `workspaces.status_changed_at`, nanoseconds, written by
  `wsm.SetStatusChangedAt`, which only ever moves it forward.
- The row resolver stamps the live instant the moment a row's published arm
  moves (`sidebar.resolver.noteStatusChange`, beside `logArmChange`) and
  hands it to the status change sink once the lock is released, the same way
  the last turn result reaches `wsm.SetResult`. `agent-repld.rosterStatusChanges`
  is the sink; a failed write is recorded at ERROR as
  `daemon.cmd.roster_status_changes`.
- The row states the later of the live and the durable instant, so a restart
  still ranks a workspace by the status change that last moved it. A daemon's
  first render of a row is not a change, so a restart stamps nothing.

### The editor

- `lisp/wire-roster.el` decodes it as `:last-updated`.
- `agent-repl--switch-by-recency` orders both `SPC p p` and `SPC p P` by it,
  newest first, keeping the roster's order between ties. A live local
  workspace no roster push carries yet ranks first: it is being registered
  now.
- The completion table states identity `display-sort-function` and
  `cycle-sort-function`, so a completion UI never re-sorts the order.
