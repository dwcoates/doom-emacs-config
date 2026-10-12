package wsm

import (
	"context"
	"database/sql"
	"fmt"

	"claude-repld/internal/dlog"
)

// THE BRANCH'S COMMIT COUNT, as the roster's detail panel states it
// (frontend.v1.RosterRowDetailCommits): how many commits the workspace's
// branch holds beyond its merge-base with the branch it was cut from.
//
// TWO WRITERS, AND THE DATABASE KEEPS THEM APART. A live workspace is
// recounted when one of its turns closes (SetLiveCommitCount); a merged one
// carries the count its merge brought in, written in the same statement that
// stamps merged_at (SetMergedAt). Once merged_at is set the branch is inside
// its parent, so a live recount would read zero: the live write is refused for
// a merged row inside its own transaction, which makes a recount racing the
// landing unable to overwrite the landed count, whatever the schedule.

// commitCountDDL adds workspaces.commit_count. Every existing row is NULL,
// which reads exactly as "never counted": the panel draws no count line until
// the workspace's next turn ends.
//
// The fresh-file schema appends this same text (schema.go), so a migrated file
// and a created one cannot differ.
const commitCountDDL = `
ALTER TABLE workspaces ADD COLUMN commit_count INTEGER;
`

// SetLiveCommitCount records a live workspace's commit count. It answers false,
// writing nothing, when the workspace has merged: a merged workspace keeps the
// count its merge landed. An unknown workspace is ErrNotFound.
func (s *store) SetLiveCommitCount(ctx context.Context, id WorkspaceID, count uint32) (bool, error) {
	fields := dlog.Context{"workspace": string(id), "commit_count": count}
	applied := false
	err := s.write(ctx, "daemon.wsm.set_live_commit_count", fields, func(ctx context.Context, tx *sql.Tx) error {
		var merged sql.NullInt64
		if err := scanWorkspaceRow(ctx, tx, id, "merged_at", &merged); err != nil {
			return err
		}
		if merged.Valid {
			return nil
		}
		if _, err := tx.ExecContext(ctx, `UPDATE workspaces SET commit_count = ? WHERE id = ?`, int64(count), id); err != nil {
			return err
		}
		applied = true
		return nil
	})
	return applied, err
}

// decodeCommitCount reads the commit_count column: NULL is "never counted",
// and a value outside uint32 is a corrupt row, never a clamped one.
func decodeCommitCount(id WorkspaceID, raw sql.NullInt64) (*uint32, error) {
	if !raw.Valid {
		return nil, nil
	}
	if raw.Int64 < 0 || raw.Int64 > int64(^uint32(0)) {
		return nil, &DecodeError{Table: "workspaces", Row: string(id), Field: "commit_count", Err: fmt.Errorf("a commit count is a uint32, got %d", raw.Int64)}
	}
	count := uint32(raw.Int64)
	return &count, nil
}
