package wsm

// This file is the store's ASYNC MERGE REQUESTS: every async merge asked for
// and not yet handed to its merge workspace, each a row of its own.
//
// A REQUESTER HOLDS ANY NUMBER OF THEM (owner ruling, 2026-11-12). They once
// rode the requester's one merge_queue row, keyed (repo_key, workspace_id), so
// a turn could ask for one async merge only, and a second was refused
// (async_merge_requester_queued). A request is keyed here by its own identity
// (MergeSource.Detach), so asking for one never touches the requester's own
// merge_queue row, and a requester's own merge and its async requests stand
// side by side. merge_queue keeps its invariant of one row per workspace, and
// every row there is a workspace's own merge: no merge_queue row is ever a
// detached request (requestMerge refuses one).
//
// THE REQUESTER'S ROW OWNS ITS REQUESTS: requester_id cascades, so a requester
// removed takes every request it had not handed off yet with it.
//
// THE ORDER IS THE ORDER THEY WERE ASKED FOR (seq), and the requesting turn is
// recorded with each (turn_id), so the hand-off moves exactly the requests
// whose turn has ended, oldest first (merge.handOffReady).

// asyncMergeRequestsDDL is the layout-128 table. Kept apart for the reason
// feedTextScaleDDL is: a fresh file and the 127 -> 128 migration write the one
// text.
const asyncMergeRequestsDDL = `
CREATE TABLE async_merge_requests (
  request_id                 TEXT PRIMARY KEY,
  repo_key                   TEXT NOT NULL,
  requester_id               TEXT NOT NULL REFERENCES workspaces(id) ON DELETE CASCADE,
  seq                        INTEGER NOT NULL,
  turn_id                    TEXT,
  requested_at               INTEGER NOT NULL,
  branch                     TEXT NOT NULL,
  name                       TEXT NOT NULL,
  maker                      TEXT NOT NULL,
  target_maker               INTEGER NOT NULL,
  pick                       TEXT,
  compact_after              INTEGER NOT NULL,
  close_on_landing           TEXT,
  cleanup_worktree           INTEGER NOT NULL,
  cleanup_dir                TEXT,
  front                      INTEGER NOT NULL,
  behind                     TEXT,
  version_part               INTEGER,
  changelog_line             TEXT,
  pre_deploy_hooks           TEXT NOT NULL,
  post_deploy_hooks          TEXT NOT NULL,
  waits_on                   TEXT,
  waits_on_name              TEXT,
  waits_on_left              TEXT
);
CREATE UNIQUE INDEX async_merge_requests_seq ON async_merge_requests(seq);
CREATE INDEX async_merge_requests_by_requester ON async_merge_requests(requester_id, seq);
`

// moveAsyncMergeRequestsDML moves every detached request still on a
// requester's merge_queue row into a row of its own, in the order the queue
// holds them, and drops it from merge_queue. A layout-127 file recorded no
// requesting turn, so each moved request has none (AsyncMergeRequest.Turn).
const moveAsyncMergeRequestsDML = `
INSERT INTO async_merge_requests (request_id, repo_key, requester_id, seq, turn_id, requested_at, branch, name, maker, target_maker, pick, compact_after, close_on_landing, cleanup_worktree, cleanup_dir, front, behind, version_part, changelog_line, pre_deploy_hooks, post_deploy_hooks, waits_on, waits_on_name, waits_on_left)
SELECT source_detach_id, repo_key, workspace_id, ROW_NUMBER() OVER (ORDER BY repo_key, seq), NULL, enqueued_at, source_branch, source_detach_name, source_detach_maker, source_detach_target_maker, source_pick, source_compact_after, source_close_on_landing, source_cleanup_worktree, source_cleanup_dir, source_front, source_behind, source_version_part, source_changelog_line, source_pre_deploy_hooks, source_post_deploy_hooks, waits_on, waits_on_name, waits_on_left
  FROM merge_queue WHERE source_detach_id IS NOT NULL;
DELETE FROM merge_queue WHERE source_detach_id IS NOT NULL;
`

// asyncMergeRequestColumns are the columns scanAsyncMergeRequests scans, in
// its order.
const asyncMergeRequestColumns = `request_id, repo_key, requester_id, turn_id, requested_at, branch, name, maker, target_maker, pick, compact_after, close_on_landing, cleanup_worktree, cleanup_dir, front, behind, version_part, changelog_line, pre_deploy_hooks, post_deploy_hooks, waits_on, waits_on_name, waits_on_left, profile_gate`
