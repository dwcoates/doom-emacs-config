# Recency gutter on the sidebar's open rows

The owner asked for every tab the recency walk (`s-[` older, `s-]` newer) can
reach to wear a colored gutter bar on its Open-section row, left of its tab
number: canonical orange when `s-[` reaches it in fewer presses, canonical
purple when `s-]` does, at most the six nearest each way, and the fewer
presses the closer to the full color.

## Assumptions (autonomous design)

- The walk's order is Emacs's alone: its selection history filters visits by
  their dwell, which the daemon never sees. So Emacs computes the reach and the
  daemon relays it. Evidence tier: existing code (`lisp/roster.el`
  `agent-repl-roster-selection-recency-order`, `lisp/workspace.el`
  `agent-repl--selection-history`).
- "At most 12 gutter entries, the 6 most reached in fewest keystrokes of
  either" is read as six per direction: the six tabs `s-[` reaches soonest and
  the six `s-]` reaches soonest, each tab assigned to the direction that
  reaches it in fewer presses. A tab equally far both ways goes to `s-[`.

## Landed changes

### `agentrepl.v1.ReportRecencyReach` (HOST)

- What: Emacs reports, replaced whole, each nearby tab's direction and press
  count. The reach is scoped to Emacs's `WatchDaemon` stream exactly as its
  desktop focus is (`streamslot`): attach starts empty, the stream's end clears
  it, a later stream supersedes an earlier one, and a report with no stream is
  refused `no_emacs_stream`.
- Why: the sidebar must never draw a reach no connected Emacs walks, and a
  stale stream's end must never erase its successor's reach. Tying the state
  to the stream makes both impossible rather than unlikely.
- Rejected: Emacs sending the whole ring and the daemon computing presses.
  That would duplicate the walk's stepping (drawn-only landing, wrap,
  pending-selection start) in Go, two copies free to drift; the gutter would
  then lie about where a press lands.
- Consequence: Emacs re-reports on every link-up, as its focus does.

### `frontend.v1.RosterOpenRow.recency_gutter`

- What: an optional `RosterOpenRowRecencyGutter` whose arm is the direction and
  whose message carries the press count.
- Why: the gutter is one drawn box per row, so one element message; the arm is
  the color family, the count the shade. The shade itself is presentation the
  webapp maps from the count.
