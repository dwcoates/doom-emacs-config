package wsm

import (
	"context"
	"database/sql"
	"errors"
	"fmt"
	"time"

	"agent-repld/internal/dlog"
)

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

// NewDetachedMergeID mints one detached merge request's identity.
func NewDetachedMergeID() DetachedMergeID { return DetachedMergeID(mint()) }

// MergeWorkspaceFor loads the merge workspace forked for one detached request.
// The bool is false when none has been registered for it yet: the request has
// not been handed off, or its fork died before registration.
func (s *store) MergeWorkspaceFor(ctx context.Context, request DetachedMergeID) (Workspace, bool, error) {
	const op = "daemon.wsm.merge_workspace_for"
	var (
		out   Workspace
		found bool
	)
	err := s.read(ctx, op, dlog.Context{"request": string(request)}, func(ctx context.Context) error {
		ws, err := scanWorkspace(s.db().QueryRowContext(ctx, `SELECT `+workspaceColumns+` FROM workspaces WHERE merge_request_id = ?`, request))
		switch {
		case errors.Is(err, sql.ErrNoRows):
			return nil
		case err != nil:
			return err
		}
		out, found = ws, true
		return nil
	})
	if err != nil {
		return Workspace{}, false, err
	}
	return out, found, nil
}

// HandOffDetachedMerge moves a detached request from the requester's queue row
// onto the merge workspace forked for it, in ONE transaction: the requester's
// REQUESTED row (which must still carry `request`) is deleted, and the merge
// workspace's own-branch, in-place, close-on-landing request is written in its
// place at the back of the queue, still REQUESTED, carrying the request's PICK
// when it has one (the merge workspace's pick turn has not begun). The caller
// then runs the pick, if any, and puts the merge workspace's request in line
// as any other. The requester's reserved name leaves with its row: the merge
// workspace now exists under it.
//
// Either both rows change or neither does, so a crash leaves the request on
// exactly one workspace: on the requester (the hand-off is redone, and finds
// the merge workspace already forked by MergeWorkspaceFor) or on the merge
// workspace (the hand-off is done).
func (s *store) HandOffDetachedMerge(ctx context.Context, repo RepoKey, requester WorkspaceID, request DetachedMergeID, mergeWS WorkspaceID, branch, pick string, at time.Time) error {
	const op = "daemon.wsm.hand_off_detached_merge"
	key, err := normalizeRepoKey(repo)
	if err != nil {
		s.log.Error(op, "refused a merge queue key that cannot be normalized", withError(dlog.Context{"repo": string(repo), "workspace": string(requester)}, err))
		return err
	}
	fields := dlog.Context{"repo": string(key), "requester": string(requester), "request": string(request), "merge_workspace": string(mergeWS), "branch": branch}
	return s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		var (
			state   int64
			detach  sql.NullString
			front   bool
			behind  sql.NullString
			verPart sql.NullInt64
			verLine sql.NullString
		)
		err := tx.QueryRowContext(ctx, `SELECT state, source_detach_id, source_front, source_behind, source_version_part, source_changelog_line FROM merge_queue WHERE repo_key = ? AND workspace_id = ?`, key, requester).Scan(&state, &detach, &front, &behind, &verPart, &verLine)
		switch {
		case errors.Is(err, sql.ErrNoRows):
			return fmt.Errorf("wsm: hand-off of detached merge %s: workspace %s has no merge request: %w", request, requester, ErrNotFound)
		case err != nil:
			return err
		}
		if MergeQueueState(state) != MergeRequested || DetachedMergeID(detach.String) != request {
			return fmt.Errorf("wsm: hand-off of detached merge %s: workspace %s's merge is %s for request %q, not the requested detached merge",
				request, requester, MergeQueueState(state), detach.String)
		}
		res, err := tx.ExecContext(ctx, `DELETE FROM merge_queue WHERE repo_key = ? AND workspace_id = ?`, key, requester)
		if err != nil {
			return err
		}
		if err := requireOneRow(res, fmt.Sprintf("wsm: merge request of workspace %s", requester)); err != nil {
			return err
		}
		version, err := scanVersion(verPart, verLine)
		if err != nil {
			return fmt.Errorf("wsm: hand-off of detached merge %s: workspace %s's request: %w", request, requester, err)
		}
		// THE MERGE WORKSPACE CARRIES THE REQUEST'S PLACE IN LINE AND ITS
		// VERSION INCREMENT: a detached merge asked for the front still goes
		// to the front, and the version it asked for is applied where it lands.
		return insertRequest(ctx, tx, key, mergeWS, MergeSource{Kind: MergeSourceOwnBranch, Branch: branch, Front: front, Behind: WorkspaceID(behind.String), Pick: pick, Version: version}, at)
	})
}

// RecordMergeWorkspaceRemoved records that the inactivity sweep is removing a
// merge workspace, BEFORE the workspace's records are deleted: a bubble that
// names a workspace with no record left reads when it went from here.
// Recording a removal twice keeps the first instant.
func (s *store) RecordMergeWorkspaceRemoved(ctx context.Context, ws WorkspaceID, requester WorkspaceID, at time.Time) error {
	const op = "daemon.wsm.record_merge_workspace_removed"
	fields := dlog.Context{"workspace": string(ws), "requester": string(requester), "removed_at": at}
	return s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		_, err := tx.ExecContext(ctx,
			`INSERT INTO removed_merge_workspaces (workspace_id, merge_of, removed_at) VALUES (?, ?, ?)
			 ON CONFLICT(workspace_id) DO NOTHING`, ws, requester, nanos(at))
		return err
	})
}

// MergeWorkspaceRemovedAt answers when the sweep removed a merge workspace; the
// bool is false when no removal is recorded for it.
func (s *store) MergeWorkspaceRemovedAt(ctx context.Context, ws WorkspaceID) (time.Time, bool, error) {
	const op = "daemon.wsm.merge_workspace_removed_at"
	var (
		at    time.Time
		found bool
	)
	err := s.read(ctx, op, dlog.Context{"workspace": string(ws)}, func(ctx context.Context) error {
		var removed int64
		err := s.db().QueryRowContext(ctx, `SELECT removed_at FROM removed_merge_workspaces WHERE workspace_id = ?`, ws).Scan(&removed)
		switch {
		case errors.Is(err, sql.ErrNoRows):
			return nil
		case err != nil:
			return err
		}
		at, found = fromNanos(removed), true
		return nil
	})
	if err != nil {
		return time.Time{}, false, err
	}
	return at, found, nil
}
