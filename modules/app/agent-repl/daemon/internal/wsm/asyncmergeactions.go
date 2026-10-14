package wsm

// This file is the durable half of an ASYNC MERGE'S OWN MERGE ACTIONS
// (agentrepl.v1.MergeWorkspaceAsync.merge_actions): the before and after
// prompts the merge workspace runs. They ride the requester's async request
// (MergeSource.Actions) until the merge workspace is forked, so a restart
// between the request and its hand-off loses neither; the hand-off carries
// them onto the merge workspace's origin (MergeOrigin.Actions), and the merge
// workspace's create records them in its own creation job, which is what its
// merge runs by. The merge workspace's own request row carries none.

// asyncMergeActionsDDL is the layout-51 addition. It adds:
//
//   - merge_queue.source_actions_before: the before-merge prompts an async
//     request names, as the JSON array creation_jobs.actions_before holds.
//   - merge_queue.source_actions_after: its after-merge prompts, the same way.
//
// IT IS ADDITIVE: the previous build's INSERTs name neither column, so a row
// it writes takes '[]', a request with no merge action, which is exactly what
// that build asked for. The fresh-file schema appends this same text
// (schema.go), so a migrated file and a created one cannot differ.
const asyncMergeActionsDDL = `
ALTER TABLE merge_queue ADD COLUMN source_actions_before TEXT NOT NULL DEFAULT '[]';
ALTER TABLE merge_queue ADD COLUMN source_actions_after TEXT NOT NULL DEFAULT '[]';
`
