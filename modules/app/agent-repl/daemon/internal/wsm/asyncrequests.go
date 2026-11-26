package wsm

// This file holds the layout-128 ASYNC MERGE REQUESTS table and the
// layout-128 step that filled it, kept because the migration list replays
// them. Nothing reads or writes the table any more: layout 222 retired the
// async merge and dropped it (retiredAsyncMergesDDL).

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
