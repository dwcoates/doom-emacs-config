# The sidebar's view state is the daemon's, shared by every page

Owner rulings, 2026-10-06:

- "The folded/unfolded status in the sidebar should be completely not specific to workspace."
- Widened the same day: ALL sidebar view state is global, and the sidebar must look exactly the same in every workspace's page — section folds, repository collapse, row expansion and the grouping shown.
- Then: open dropdowns — the workspace status indicator's dropdown (the row's detail popover) and the row's triple-dots menu among them — are transient, NOT the shared view state.
- Actions about one workspace are not view state either.

Each workspace has its own webview page, and the rail kept task and merged folds, the grouping and a row's open detail in that page's `localStorage`, read once at page creation, so one page's change never reached the others.

## Landed changes

1. `frontend.v1.WorkspaceRoster` gains `oneof shown { RosterShownRepository shown_repository = 5; RosterShownTask shown_task = 6; }`.
   - Never chosen by anyone, it is the repository grouping.

2. `frontend.v1.RosterTaskSection` gains `oneof fold { RosterTaskSectionExpanded expanded = 4; RosterTaskSectionCollapsed collapsed = 5; }`.
   - A task never folded is expanded.

3. `frontend.v1.RosterMergedSection` gains `oneof fold { RosterMergedSectionExpanded expanded = 3; RosterMergedSectionCollapsed collapsed = 4; }`.
   - Never folded or unfolded by anyone, it is collapsed.

4. New verb `agentrepl.v1.UpdateSidebarView`, `oneof change`:
   - `fold_section`: `oneof section { workspace.v1.RepositoryRef repository; TaskRef task; SidebarViewRecentlyMerged recently_merged }` and `oneof fold { expand; collapse }`.
   - `show_grouping`: `oneof grouping { repository; task }`.
   - Response `oneof result { success; error }`, error `oneof cause { unknown_repository; unknown_task }`.
   - Unset arms and blank refs are Connect InvalidArgument; a change to what the view already holds is success.

5. `FoldRepository` is marked SUPERSEDED in its comment; no client calls it any more.

## Why these shapes

- Each piece of view state rides the box it styles ("the message tree is the UI tree"), and the roster push is already the one record every page watches, so the view and the rows it shapes can never arrive out of step.
- One verb with a `change` oneof rather than a verb per preference, at the owner's direction.
- Every two-state fact is a two-arm oneof of empty messages; no bools, no state enums.
- Per-section fold arm messages mirror the existing `RosterRepoSection.fold`, whose arms carry tab-bar semantics.

## Not view state

- A row's detail popover ("row expansion") was carried as `RosterRow.detail_open` with a `set_row_detail` arm until the dropdown ruling named the status indicator's dropdown transient; both were removed before landing, and the popover's openness is the page's own.
- Which "row expansion" the widened ruling meant is the one question this leaves: the only row expansion the rail has is that popover.

## Open for the owner

- Retiring `FoldRepository` outright: it is a breaking change (an rpc removed), so it is left in place and marked superseded.
