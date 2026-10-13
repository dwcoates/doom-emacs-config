package wsm

import (
	"context"
	"database/sql"
	"errors"
	"time"

	"agent-repld/internal/dlog"
)

// This file is the durable half of a detached merge's LANDING ON ITS
// REQUESTER. A detached merge lands from the merge workspace, so the requester
// is never stamped merged and keeps its worktree; what it records instead is
// which commit of its branch landed, and when. The merge workspace sweep
// (mergeworkspace.Service) retires a requester whose branch still stands at
// that commit once it has been idle a day.

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

// Landing is a requester's record of its detached merge landing.
type Landing struct {
	// Head is the commit of the requester's branch that landed: the merge
	// workspace's fork head.
	Head string
	// At is when the merge landed.
	At time.Time
}

// SetLanded records, or clears with nil, a requester's landing. An empty head
// or a zero instant is refused: a landing names both. An unknown workspace is
// ErrNotFound.
func (s *store) SetLanded(ctx context.Context, id WorkspaceID, landing *Landing) error {
	const op = "daemon.wsm.set_landed"
	fields := dlog.Context{"workspace": string(id)}
	if landing == nil {
		fields["landed"] = nil
		return s.setWorkspaceColumns(ctx, op, id, fields,
			workspaceColumn{name: "landed_head", value: nil},
			workspaceColumn{name: "landed_at", value: nil})
	}
	fields["landed_head"], fields["landed_at"] = landing.Head, landing.At
	if landing.Head == "" || landing.At.IsZero() {
		err := errors.New("wsm: a landing names both the commit that landed and when")
		s.log.Error(op, "refused an incomplete landing", withError(fields, err))
		return err
	}
	return s.setWorkspaceColumns(ctx, op, id, fields,
		workspaceColumn{name: "landed_head", value: landing.Head},
		workspaceColumn{name: "landed_at", value: nanos(landing.At)})
}

// decodeLanding decodes the landing columns all-or-nothing: one set without
// the other is a corrupt row.
func decodeLanding(id WorkspaceID, head sql.NullString, at sql.NullInt64) (*Landing, error) {
	if head.Valid != at.Valid {
		return nil, &DecodeError{Table: "workspaces", Row: string(id), Field: "landed_head", Err: errors.New("a landing records both its commit and its instant")}
	}
	if !head.Valid {
		return nil, nil
	}
	return &Landing{Head: head.String, At: fromNanos(at.Int64)}, nil
}
