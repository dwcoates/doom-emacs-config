package wsm

// This file is the durable half of an ASYNC MERGE'S LANDING EFFECTS
// (agentrepl.v1.MergeWorkspaceAsync.one_shot and .cleanup_worktree): the
// workspace an async merge closes once it lands, and whether it removes the
// worktree that had its branch checked out. Both ride the requester's async
// request (MergeSource.CloseOnLanding, .CleanupWorktree, .CleanupDir) until
// the merge workspace is forked, and the merge workspace's origin
// (MergeOrigin) from then on, which is what the landing reads.

// asyncLandingDDL is the layout-45 addition. It adds:
//
//   - merge_queue.source_close_on_landing: the workspace an async request
//     closes once it lands; NULL on every existing row, none of which asked.
//   - merge_queue.source_cleanup_worktree: whether it removes worktrees once
//     it lands; 0 on every existing row.
//   - merge_queue.source_cleanup_dir: the worktree that had the request's
//     branch checked out when the request was accepted, removed on landing;
//     NULL when none qualified.
//   - workspaces.merge_close_on_landing, merge_cleanup_worktree,
//     merge_cleanup_dir: the same three on a MERGE WORKSPACE's origin.
//
// IT IS ADDITIVE: the previous build's INSERTs name none of the columns, so a
// row it writes takes the defaults, which are exactly what that build asked
// for. The fresh-file schema appends this same text (schema.go), so a migrated
// file and a created one cannot differ.
const asyncLandingDDL = `
ALTER TABLE merge_queue ADD COLUMN source_close_on_landing TEXT;
ALTER TABLE merge_queue ADD COLUMN source_cleanup_worktree INTEGER NOT NULL DEFAULT 0;
ALTER TABLE merge_queue ADD COLUMN source_cleanup_dir TEXT;
ALTER TABLE workspaces ADD COLUMN merge_close_on_landing TEXT;
ALTER TABLE workspaces ADD COLUMN merge_cleanup_worktree INTEGER NOT NULL DEFAULT 0;
ALTER TABLE workspaces ADD COLUMN merge_cleanup_dir TEXT;
`
