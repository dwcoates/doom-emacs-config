package wsm

import (
	"context"
	"database/sql"
	"fmt"
	"time"

	"agent-repld/internal/dlog"
)

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

// AsyncMergeRequest is one async merge asked for and not yet handed to its
// merge workspace.
type AsyncMergeRequest struct {
	// Repo is the queue the merge lands through.
	Repo RepoKey
	// Requester is the workspace that asked for it.
	Requester WorkspaceID
	// Turn is the requester's turn that asked for it, empty when none was in
	// flight (or a layout-127 file recorded the request): the hand-off waits
	// for the requester's turn in flight, whichever it is.
	Turn TurnID
	// RequestedAt is when it was asked for.
	RequestedAt time.Time
	// Source is the request: a named branch, detached (Source.Detach is the
	// request's identity), carrying every landing effect it asked for.
	Source MergeSource
	// WaitsOn, WaitsOnName and WaitsOnLeft are the request's wait on the merge
	// it was placed behind, as MergeQueueEntry's: carried onto the merge
	// workspace's own entry at the hand-off.
	WaitsOn     WorkspaceID
	WaitsOnName string
	WaitsOnLeft string
}

// RequestAsyncMerge records one async merge request of REQUESTER, asked for
// in TURN (empty when none was in flight), behind every request recorded
// before it. A request whose landing would close a workspace with open
// children is refused (HasOpenChildrenError, family.go).
func (s *store) RequestAsyncMerge(ctx context.Context, repo RepoKey, requester WorkspaceID, turn TurnID, source MergeSource, at time.Time) error {
	const op = "daemon.wsm.request_async_merge"
	key, err := normalizeRepoKey(repo)
	if err != nil {
		s.log.Error(op, "refused a merge queue key that cannot be normalized", withError(dlog.Context{"repo": string(repo), "workspace": string(requester)}, err))
		return err
	}
	fields := dlog.Context{"repo": string(key), "workspace": string(requester), "request": string(source.Detach), "turn": string(turn), "requested_at": at}
	// AN ASYNC REQUEST IS A DETACHED NAMED BRANCH: the orchestrator builds it
	// so, so one that is not is a defect in that caller.
	if err := source.validate(); err != nil || source.Detach == "" || source.Kind != MergeSourceBranch {
		if err == nil {
			err = fmt.Errorf("an async merge request is a detached named branch, not a %s source with detach %q", source.Kind, source.Detach)
		}
		s.log.Error(op, "refused an async merge request whose source contradicts itself", withError(fields, err))
		return fmt.Errorf("wsm: async merge request of %s: %w", requester, err)
	}
	return s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		// A MERGE NEVER CLOSES A PARENT OVER ITS OPEN CHILDREN (family.go).
		origin, err := mergeOriginOf(ctx, tx, requester)
		if err != nil {
			return err
		}
		for _, closes := range source.Closes(requester, origin) {
			if err := refuseOpenChildren(ctx, tx, closes); err != nil {
				return err
			}
		}
		var last sql.NullInt64
		if err := tx.QueryRowContext(ctx, `SELECT max(seq) FROM async_merge_requests`).Scan(&last); err != nil {
			return err
		}
		before, err := encodeHooks(source.Hooks.PreDeploy)
		if err != nil {
			return err
		}
		after, err := encodeHooks(source.Hooks.PostDeploy)
		if err != nil {
			return err
		}
		_, err = tx.ExecContext(ctx,
			`INSERT INTO async_merge_requests (request_id, repo_key, requester_id, seq, turn_id, requested_at, branch, name, maker, target_maker, pick, compact_after, close_on_landing, cleanup_worktree, cleanup_dir, front, behind, version_part, changelog_line, pre_deploy_hooks, post_deploy_hooks, waits_on, waits_on_name)
			 VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)`,
			source.Detach, key, requester, last.Int64+1, nullableText(string(turn)), nanos(at), source.Branch, source.DetachName, source.DetachMaker,
			source.DetachTargetMaker, nullableText(source.Pick), source.CompactAfter, nullableText(string(source.CloseOnLanding)), source.CleanupWorktree,
			nullableText(source.CleanupDir), source.Front, nullableText(string(source.Behind)), versionPart(source.Version), versionLine(source.Version),
			before, after, nullableText(string(source.Behind)), nullableText(source.BehindName))
		return err
	})
}

// AsyncMergeRequestsOf answers REQUESTER's requests, in the order they were
// asked for.
func (s *store) AsyncMergeRequestsOf(ctx context.Context, requester WorkspaceID) ([]AsyncMergeRequest, error) {
	const op = "daemon.wsm.async_merge_requests_of"
	var out []AsyncMergeRequest
	err := s.read(ctx, op, dlog.Context{"workspace": string(requester)}, func(ctx context.Context) error {
		var err error
		out, err = s.scanAsyncMergeRequests(ctx, `WHERE requester_id = ? ORDER BY seq`, requester)
		return err
	})
	if err != nil {
		return nil, err
	}
	return out, nil
}

// AllAsyncMergeRequests answers every request standing, in the order they were
// asked for: the boot's recovery re-arms each requester's hand-off from them.
// One that will not decode loads none.
func (s *store) AllAsyncMergeRequests(ctx context.Context) ([]AsyncMergeRequest, error) {
	const op = "daemon.wsm.all_async_merge_requests"
	var out []AsyncMergeRequest
	err := s.read(ctx, op, dlog.Context{}, func(ctx context.Context) error {
		var err error
		out, err = s.scanAsyncMergeRequests(ctx, `ORDER BY seq`)
		return err
	})
	if err != nil {
		return nil, err
	}
	return out, nil
}

// asyncMergeRequestColumns are the columns scanAsyncMergeRequests scans, in
// its order.
const asyncMergeRequestColumns = `request_id, repo_key, requester_id, turn_id, requested_at, branch, name, maker, target_maker, pick, compact_after, close_on_landing, cleanup_worktree, cleanup_dir, front, behind, version_part, changelog_line, pre_deploy_hooks, post_deploy_hooks, waits_on, waits_on_name, waits_on_left`

// scanAsyncMergeRequests is the one decoding path for async_merge_requests;
// TAIL is the read's WHERE and ORDER BY.
func (s *store) scanAsyncMergeRequests(ctx context.Context, tail string, args ...any) ([]AsyncMergeRequest, error) {
	rows, err := s.q(ctx).QueryContext(ctx, `SELECT `+asyncMergeRequestColumns+` FROM async_merge_requests `+tail, args...)
	if err != nil {
		return nil, err
	}
	defer rows.Close()
	var out []AsyncMergeRequest
	for rows.Next() {
		var (
			req       AsyncMergeRequest
			id        string
			turn      sql.NullString
			at        int64
			pick      sql.NullString
			closes    sql.NullString
			cleanDir  sql.NullString
			behind    sql.NullString
			verPart   sql.NullInt64
			verLine   sql.NullString
			before    string
			after     string
			waitsOn   sql.NullString
			waitsName sql.NullString
			waitsLeft sql.NullString
		)
		src := &req.Source
		if err := rows.Scan(&id, &req.Repo, &req.Requester, &turn, &at, &src.Branch, &src.DetachName, &src.DetachMaker, &src.DetachTargetMaker, &pick,
			&src.CompactAfter, &closes, &src.CleanupWorktree, &cleanDir, &src.Front, &behind, &verPart, &verLine, &before, &after,
			&waitsOn, &waitsName, &waitsLeft); err != nil {
			return nil, err
		}
		row := id
		src.Kind = MergeSourceBranch
		src.Detach = DetachedMergeID(id)
		src.Pick, src.CloseOnLanding, src.CleanupDir = pick.String, WorkspaceID(closes.String), cleanDir.String
		src.Behind, src.BehindName = WorkspaceID(behind.String), waitsName.String
		if src.Behind == "" {
			src.BehindName = ""
		}
		if src.Version, err = scanVersion(verPart, verLine); err != nil {
			return nil, &DecodeError{Table: "async_merge_requests", Row: row, Field: "version", Err: err}
		}
		if src.Hooks.PreDeploy, err = decodeHooks("async_merge_requests", row, "pre_deploy_hooks", before); err != nil {
			return nil, err
		}
		if src.Hooks.PostDeploy, err = decodeHooks("async_merge_requests", row, "post_deploy_hooks", after); err != nil {
			return nil, err
		}
		if err := src.validate(); err != nil {
			return nil, &DecodeError{Table: "async_merge_requests", Row: row, Field: "source", Err: err}
		}
		req.Turn = TurnID(turn.String)
		req.RequestedAt = fromNanos(at)
		req.WaitsOn, req.WaitsOnName, req.WaitsOnLeft = WorkspaceID(waitsOn.String), waitsName.String, waitsLeft.String
		if req.WaitsOnLeft != "" && req.WaitsOn == "" {
			return nil, &DecodeError{Table: "async_merge_requests", Row: row, Field: "waits_on_left", Err: fmt.Errorf("records the cause %q a merge ahead left for, but waits on no merge", req.WaitsOnLeft)}
		}
		out = append(out, req)
	}
	if err := rows.Err(); err != nil {
		return nil, err
	}
	return out, nil
}

// DropAsyncMergeRequest removes one request that will never be handed off (its
// merge workspace could not be forked), settling every merge placed behind it
// with CAUSE (settleWaitsOn) in the same write. Dropping a request no longer
// standing is ErrNotFound.
func (s *store) DropAsyncMergeRequest(ctx context.Context, repo RepoKey, request DetachedMergeID, cause string) error {
	const op = "daemon.wsm.drop_async_merge_request"
	key, err := normalizeRepoKey(repo)
	if err != nil {
		s.log.Error(op, "refused a merge queue key that cannot be normalized", withError(dlog.Context{"repo": string(repo), "request": string(request)}, err))
		return err
	}
	fields := dlog.Context{"repo": string(key), "request": string(request), "cause": cause}
	var settled int64
	err = s.writeQueue(ctx, op, fields, key, func(ctx context.Context, tx *sql.Tx) error {
		res, err := tx.ExecContext(ctx, `DELETE FROM async_merge_requests WHERE request_id = ? AND repo_key = ?`, request, key)
		if err != nil {
			return err
		}
		if err := requireOneRow(res, fmt.Sprintf("wsm: async merge request %s in repo %q", request, key)); err != nil {
			return err
		}
		settled, err = settleWaitsOn(ctx, tx, key, WorkspaceID(request), cause)
		return err
	})
	if err != nil {
		return err
	}
	s.logSettledWaits(op, fields, settled)
	return nil
}
