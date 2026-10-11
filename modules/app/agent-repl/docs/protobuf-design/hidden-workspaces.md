# Hidden workspaces

The owner asked (2026-10-11) for a HIDDEN WORKSPACES section in the sidebar. A
workspace in it is not drawn on the Emacs tab bar, so the tab numbers shift to
the next workspace over, using exactly the machinery a collapsed repository
section already uses. The section itself is not drawn when nothing is hidden.
`SPC TAB h` hides the current workspace, and clicking a row in the hidden
section shows it again.

The owner asked for the work to be planned and executed without waiting on
design rounds, so the shapes below were settled by following the nearest
precedents (`frontend.v1.RosterMergedSection`, `agentrepl.v1.SetWorkspacePriority`,
`agentrepl.v1.SidebarViewRecentlyMerged`) rather than through per-increment
agreement.

## Landed changes

### `agentrepl.v1.SetWorkspaceHidden`

- WHAT: a new SIDEBAR rpc whose request carries the `workspace.v1.WorkspaceRef`
  and a `visibility` oneof of `hide` / `show`. Its error arms are the same four
  ownership refusals `SetWorkspacePriority` carries.
- WHY: hidden is a per-workspace durable fact (it survives a daemon restart),
  so it is a workspace verb rather than view state; `UpdateSidebarView`'s own
  header excludes "anything that acts on one workspace".
- The mode is a two-arm oneof rather than a bool, per the bool-selects-mode
  rule.
- Consequences:
  - the daemon stores a `hidden` column on `workspaces` (layout 32, additive);
  - both clients (Emacs `SPC TAB h`, the webapp's hidden-row click) call it;
  - asking for the state a workspace already holds is a success, as
    `UpdateSidebarView` already rules for folds.

### `frontend.v1.WorkspaceRoster.hidden` / `frontend.v1.RosterHiddenSection`

- WHAT: a section shaped exactly like `RosterMergedSection` (header, rows,
  expanded/collapsed fold), always on the wire.
- WHY: the hidden workspaces are hoisted out of every grouping, so they need a
  home of their own drawn under every grouping, as the merged band is.
- Rows nest by lineage among the hidden workspaces alone (like a repository
  section), so a hidden family keeps its shape.
- A merged workspace stays in `recently_merged` whatever its hidden flag:
  merged rows are closed (no tab) already, and done is the more useful fact.
- Its fold defaults to EXPANDED, so the click that shows a workspace again is
  in reach.
- Consequences:
  - the Emacs tab bar walks the hidden section's rows as HIDDEN tabs, the same
    `:collapsed` flag a folded repository section sets, so numbering and
    navigation skip them by the existing machinery;
  - the daemon's `sidebar.TabOrder` and `rosterwalk.AllRows` walk the section
    too (a hidden workspace still has a tab and a session to bring up);
  - the Emacs roster decoder (`lisp/wire-roster.el`) refuses unknown fields, so
    it learns the new field in the same change.

### `agentrepl.v1.SidebarViewFoldSection.hidden`

- WHAT: a `SidebarViewHidden` arm on the fold's section oneof.
- WHY: the section folds like every other, and folds are the daemon's.
- Its fold is stored on the `sidebar_view` singleton as `hidden_folded`
  (layout 32, default 0 = expanded).
