# The open workspaces section

The owner asked (2026-10-13) for an "OPEN" heading over the sidebar's main
workspace list, drawn like the "HIDDEN" and "RECENTLY MERGED" headings (fold
chevron, purple uppercase label), with the number of workspaces the section
lists beside it whether the section is folded or not, in the font and color of
Recently Merged's "(K today)". From here on, documentation calls that list
the OPEN WORKSPACES SECTION of the sidebar.

The shapes follow the nearest precedents (`frontend.v1.RosterHiddenSection`,
`agentrepl.v1.SidebarViewHidden`), as `hidden-workspaces.md` did.

## Landed changes

### `frontend.v1.WorkspaceRoster.open` / `frontend.v1.RosterOpenSection`

- WHAT: the section's label and its expanded/collapsed fold, always on the wire.
- WHY: the section folds like every other section, and folds are the daemon's.
- It carries no rows: the groupings stay where they are
  (`WorkspaceRoster.repository`, `.task`, `.maturity`), and the section is the
  box whichever one is shown draws in.
- Its fold defaults to EXPANDED and folds sidebar rows only; the Emacs tab bar
  ignores it.

### `frontend.v1.RosterOpenCount` on every grouping view

- WHAT: `open_count` on `frontend.v1.RosterRepositoryView`,
  `frontend.v1.RosterTaskView` and `frontend.v1.RosterMaturityView`.
- WHY: the groupings do not list the same workspaces (the task grouping leaves
  an unassigned workspace out), so one roster-wide count would be wrong under
  some grouping.
- It is the sum of the grouping's section counts: every not-closed row,
  nested family rows included, a folded section's included. That is the number
  a person arrives at by counting the section's entries.

### `agentrepl.v1.SidebarViewFoldSection.open`

- WHAT: a `agentrepl.v1.SidebarViewOpen` arm on the fold's section oneof.
- Stored on the `sidebar_view` singleton as `open_folded` (layout 46, additive).

### `frontend.v1.RosterRowWhen.turn_running` / `.no_turn_running`

- WHAT: the open workspaces section's when-column is the running turn's
  duration, from `frontend.v1.RosterRowWhenTurnRunning.started_at_ms`, and
  `frontend.v1.RosterRowWhenNoTurnRunning` ("0s") with no turn running (owner
  request, 2026-10-13).
- WHY the start instant is the footer's: the owner asked for exactly the turn
  duration number, so the daemon reads it off the workspace's published
  `frontend.v1.FooterClock.turn_started_at_ms`, and the webapp ticks it with
  the footer clock's own formatter. The two cannot disagree.
- `turn_ended` (field 5) and `created` (field 4) are retired: no row shows
  the time since its last turn ended any more.
- The arm is never unset: every row has a precedence arm to draw.

### `frontend.v1.RosterRowWhen.sync_merging`

- WHAT: while a SYNC merge stands in the workspace (in line or being worked
  on), the when-column counts from
  `frontend.v1.RosterRowWhenSyncMerging.started_at_ms`, the instant the merge
  joined its repository's queue (owner request, 2026-10-14).
- WHY the queue instant: it is the queue entry's durable `EnqueuedAt`, so a
  daemon restart that resumes the merge does not reset the count.
- WHY it outranks `turn_running`: the turns a merge runs (its configured
  prompts, conflict resolution, fixing) are the merge's work, so the row
  counts the whole merge rather than restarting at each of them.
- WHY sync only: the daemon reads the instant off the footer's published
  merging arm (`footer.Resolver.MergeStartedAt`), and an async merge's
  requester is never told its merge, so its row keeps its turn clock.
