package wsm

// This file is the durable half of COMPACT AFTER MERGE
// (agentrepl.v1.MergeWorkspaceSourceOwnBranchDetached.compact_after): an
// async merge whose requester asked to have its conversation compacted once
// the branch lands. The ask rides the requester's detached request
// (MergeSource.CompactAfter) until the merge workspace is forked, and the
// merge workspace's origin (MergeOrigin.CompactAfter) from then on, which is
// what the landed handback reads.

// compactAfterDDL is the layout-44 addition. It adds:
//
//   - merge_queue.source_compact_after: whether a detached request compacts
//     its requester once it lands. 0 on every existing row, none of which
//     asked.
//   - workspaces.merge_compact_after: on a MERGE WORKSPACE, whether its
//     requester is compacted once it lands. 0 on every existing row.
//
// IT IS ADDITIVE: the previous build's INSERTs name neither column, so a row
// it writes takes the default 0, which is exactly what that build asked for.
// The fresh-file schema appends this same text (schema.go), so a migrated file
// and a created one cannot differ.
const compactAfterDDL = `
ALTER TABLE merge_queue ADD COLUMN source_compact_after INTEGER NOT NULL DEFAULT 0;
ALTER TABLE workspaces ADD COLUMN merge_compact_after INTEGER NOT NULL DEFAULT 0;
`
