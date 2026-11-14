package wsm

// retiredColumnsDDL is the layout-137 step: it drops the columns no statement
// of this build reads or writes, each retired by an earlier deploy that kept
// it in place so the build before it went on working (MigrationKind, A
// BREAKING CHANGE IS SHIPPED AS TWO DEPLOYS):
//
//   - workspaces.principal_link (layout 71), replaced by workspace_links
//     (layout 85).
//   - sidebar_view.open_shows_merge_queues (layout 76), the open section's
//     merge-queue toggle, retired 2026-10-26.
//   - sidebar_view.backlog_folded, in_progress_folded and merging_folded
//     (layout 29), the stage grouping's folds, and sidebar_view.grouping
//     (layout 23), the grouping shown, both retired 2026-10-15.
//   - sidebar_view.viewed_folded (layout 40), the viewed band's fold.
//
// IT IS BREAKING: the build before it still inserts sidebar_view's grouping,
// so the deploy that crosses it restarts.
const retiredColumnsDDL = `
ALTER TABLE workspaces DROP COLUMN principal_link;
ALTER TABLE sidebar_view DROP COLUMN open_shows_merge_queues;
ALTER TABLE sidebar_view DROP COLUMN backlog_folded;
ALTER TABLE sidebar_view DROP COLUMN in_progress_folded;
ALTER TABLE sidebar_view DROP COLUMN merging_folded;
ALTER TABLE sidebar_view DROP COLUMN viewed_folded;
ALTER TABLE sidebar_view DROP COLUMN grouping;
`
