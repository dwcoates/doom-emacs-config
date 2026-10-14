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
