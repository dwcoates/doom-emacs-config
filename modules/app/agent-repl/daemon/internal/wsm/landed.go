package wsm

// This file held the durable half of a detached merge's LANDING ON ITS
// REQUESTER. Layout 221 retires it with the async merge
// (retiredasyncmerges.go); its DDL stays for the layouts that replay it.

// landedDDL adds:
//
//   - workspaces.merge_fork_head: on a MERGE WORKSPACE, the commit it was cut
//     at (MergeOrigin.ForkHead). NULL on every existing row: a merge workspace
//     forked before this layout never recorded it, and its requester is never
//     stamped.
//   - workspaces.landed_head and workspaces.landed_at: set together on a
//     REQUESTER whose detached merge landed (Workspace.Landed). NULL on every
//     existing row.
//
// The fresh-file schema appends this same text (schema.go), so a migrated file
// and a created one cannot differ.
const landedDDL = `
ALTER TABLE workspaces ADD COLUMN merge_fork_head TEXT;
ALTER TABLE workspaces ADD COLUMN landed_head TEXT;
ALTER TABLE workspaces ADD COLUMN landed_at INTEGER;
`
