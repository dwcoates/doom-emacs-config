package wsm

// asyncMergeMakerDDL is the layout-61 addition: an async request's MAKER and
// TARGET (agentrepl.v1.CreateWorkspaceAsyncMerge), carried on the requester's
// detached request until its hand-off forks the merge workspace.
//
//   - source_detach_maker: the workspace whose conversation the merge
//     workspace forks -- the one the merged work was done in.
//   - source_detach_target_maker: 1 when the branch lands into the maker's
//     branch, 0 when it lands on the primary branch.
//
// IT IS ADDITIVE, and it BACKFILLS: a detached request the previous build
// recorded forked its requester and landed by its requester's geometry, so
// its maker is its requester; the target column's default (the primary
// branch) is what that build landed a top-level requester's merge on.
const asyncMergeMakerDDL = `
ALTER TABLE merge_queue ADD COLUMN source_detach_maker TEXT;
ALTER TABLE merge_queue ADD COLUMN source_detach_target_maker INTEGER NOT NULL DEFAULT 0;
UPDATE merge_queue SET source_detach_maker = workspace_id WHERE source_detach_id IS NOT NULL;
`
