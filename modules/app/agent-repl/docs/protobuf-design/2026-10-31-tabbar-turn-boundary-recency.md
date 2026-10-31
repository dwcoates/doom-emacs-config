# The one-line tab bar keeps the tabs whose turns most recently moved

The owner asked (2026-10-31) that the Emacs tab bar, shown only while the
agent-repl panels are closed, be one line instead of two, and that when the
tabs do not all fit it show the ones whose turn most recently STARTED or ENDED
(the two of equal precedence: the newer instant wins), drawn in the sidebar's
order.

## Assumptions (autonomous design)

- Objective: the editor can rank tabs by the newest turn start-or-end.
- Systems: daemon (sidebar row resolver), Emacs (roster decoder, tab bar).
  The webapp decodes the roster through the generated TS bindings and reads
  nothing new.
- No non-additive change.

## Landed changes

### `frontend.v1.RosterRow.last_turn_boundary` (tag 70)

- WHAT: `RosterRowLastTurnBoundary { at_ms }`, optional on the row.
- WHY: the row carried the last END durably (`last_turn_ended`) but a START
  only inside the when-column's `turn_running` arm, which the `sync_merging`
  and `hidden` arms outrank. Ranking by "newest start or end" in Emacs would
  have meant reading two fields and losing every turn a sync merge runs.
- The single instant suffices because turn boundaries alternate: with a turn
  running, its start is newer than every end; with none, the last end is newer
  than every start. So the daemon resolves `running start, else durable end`.
- SYNC MERGE RULING (owner, 2026-10-31): while a sync merge stands, the
  boundary is the START of the turn that finished into the merge, not that
  turn's end, and not any turn the merge itself runs. The workspace is a
  one-shot taking no prompt, so to the user its merge continues that turn.
  The instant is durable: `wsm.Workspace.MergeTurnStartedAt`, the newest of
  the workspace's turn starts at or before its own `merge_queue.enqueued_at`,
  derived on read like `LastTurnEndedAt`. Unset when no turn started before
  the entry.
- Alternatives rejected:
  - Two fields (`last_turn_started` plus the existing end), max taken by the
    client: the client would derive the ranking, and the daemon keeps no
    durable start to put in the first.
  - Elisp reading the when-column: misses the masked arms (above).
- Consequences:
  - Resolved in `daemon/internal/resolve/sidebar/rows.go` from the same
    footer clock the when-column reads (`footer.Resolver.TurnClock`), so the
    two cannot disagree about a running turn.
  - A cold submit whose route is not yet seen has no clock start, so the row
    keeps its last end until the turn's clock starts, matching the footer.
