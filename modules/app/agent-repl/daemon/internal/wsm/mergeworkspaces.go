package wsm

// This file is the durable half of DETACHED MERGES
// (agentrepl.v1.MergeWorkspaceSourceOwnBranchDetached): the detached request's
// identity on its queue row, the merge workspace's record of what it was forked
// for, the hand-off of the request to that workspace, and the record of a merge
// workspace the inactivity sweep removed.

// mergeWorkspacesDDL adds:
//
//   - merge_queue.source_detach_id: a detached request's identity, NULL for an
//     in-place merge. Every existing row is in place, which is right: no build
//     before this one ran a merge detached.
//   - workspaces.merge_of and workspaces.merge_request_id: set together on a
//     MERGE WORKSPACE, naming the requester and the detached request it was
//     forked for. NULL on every existing row, none of which is one.
//   - removed_merge_workspaces: when the inactivity sweep removed a merge
//     workspace. It references no workspace row on purpose: it is what is left
//     once that row is gone, so a merge bubble can still say the workspace it
//     names was removed and when.
//
// The fresh-file schema appends this same text (schema.go), so a migrated file
// and a created one cannot differ.
const mergeWorkspacesDDL = `
ALTER TABLE merge_queue ADD COLUMN source_detach_id TEXT;
ALTER TABLE workspaces ADD COLUMN merge_of TEXT;
ALTER TABLE workspaces ADD COLUMN merge_request_id TEXT;
CREATE TABLE removed_merge_workspaces (
  workspace_id TEXT PRIMARY KEY,
  merge_of     TEXT NOT NULL,
  removed_at   INTEGER NOT NULL
);
`
