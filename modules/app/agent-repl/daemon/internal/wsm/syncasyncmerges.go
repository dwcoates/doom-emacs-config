package wsm

// This file is the durable half of SYNC AND ASYNC MERGES
// (docs/protobuf-design/sync-async-merges.md): whether a merge workspace has
// been revealed, the name an async request reserves and the pick it may
// carry, and the mirror a merge workspace's turn is also drawn at.

// syncAsyncMergesDDL adds:
//
//   - merge_queue.source_detach_name: an async request's merge workspace name,
//     reserved while the request stands. Every async request an earlier build
//     recorded is given the name that build would have forked under
//     ("<branch>-merge"), so a request recorded across the upgrade still forks.
//   - merge_queue.source_pick, merge_queue.pick_started_at and
//     merge_queue.pick_turn: the pick instruction an async request (and then
//     its merge workspace's request) carries, and when the merge workspace's
//     pick turn began and which turn it is. NULL on every existing row, none
//     of which picks.
//   - workspaces.merge_revealed: whether a merge workspace has been revealed.
//     Every existing merge workspace was drawn like any workspace, so each is
//     migrated as revealed.
//   - turns.mirror: the mirror a turn of a merge workspace is also drawn at
//     (MirrorAddress). NULL on every existing turn.
//
// The fresh-file schema appends this same text (schema.go), so a migrated file
// and a created one cannot differ; on a fresh file both UPDATEs touch nothing.
const syncAsyncMergesDDL = `
ALTER TABLE merge_queue ADD COLUMN source_detach_name TEXT;
ALTER TABLE merge_queue ADD COLUMN source_pick TEXT;
ALTER TABLE merge_queue ADD COLUMN pick_started_at INTEGER;
ALTER TABLE merge_queue ADD COLUMN pick_turn TEXT;
UPDATE merge_queue SET source_detach_name = COALESCE(source_branch, workspace_id) || '-merge'
 WHERE source_detach_id IS NOT NULL;
ALTER TABLE workspaces ADD COLUMN merge_revealed INTEGER NOT NULL DEFAULT 0;
UPDATE workspaces SET merge_revealed = 1 WHERE merge_of IS NOT NULL;
ALTER TABLE turns ADD COLUMN mirror TEXT;
`
