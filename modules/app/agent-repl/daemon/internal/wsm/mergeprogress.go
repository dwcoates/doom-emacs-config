package wsm

import (
	"context"
	"database/sql"
	"errors"
	"fmt"
	"time"

	"claude-repld/internal/dlog"
)

// mergeProgressDDL is the layout-28 addition: an ADMITTED merge's progress,
// recorded by the orchestrator at every step boundary so a daemon restart
// resumes the merge from the step it stood on instead of guessing from the
// trees what the dead run had done (owner ruling, 2026-10-06: a merge always
// resumes where it left off).
//
// ONE ROW PER WORKSPACE, because a workspace runs at most one merge; the row
// names the merge's lease, which is also its ledger identity and its bubble's
// row identity. The document is the orchestrator's own record: this store
// keeps it whole and never reads inside it, and the orchestrator decodes it
// (refusing the boot on one that will not decode, as for every corrupt row).
//
// A new table, so the step is additive: the build before it never names it.
const mergeProgressDDL = `
CREATE TABLE merge_progress (
  workspace_id TEXT PRIMARY KEY REFERENCES workspaces(id) ON DELETE CASCADE,
  lease_id     TEXT NOT NULL,
  updated_at   INTEGER NOT NULL,
  document     TEXT NOT NULL
);
`

// MergeProgress is one admitted merge's durable progress record.
type MergeProgress struct {
	// Workspace is the requester the merge runs in.
	Workspace WorkspaceID
	// Lease is the merge's occupancy lease: its ledger and bubble identity.
	Lease LeaseID
	// UpdatedAt is when the record was last written.
	UpdatedAt time.Time
	// Document is the orchestrator's record of where the merge stands. The
	// store keeps it whole and never interprets it.
	Document []byte
}

// PutMergeProgress records a merge's progress, replacing whatever the
// workspace's row held. A record with no workspace, no lease or no document
// is a defect in the caller and is refused rather than stored: a resume read
// off it would have nothing to resume.
func (s *store) PutMergeProgress(ctx context.Context, p MergeProgress) error {
	const op = "daemon.wsm.put_merge_progress"
	fields := dlog.Context{"workspace": string(p.Workspace), "lease": string(p.Lease)}
	if p.Workspace == "" || p.Lease == "" || len(p.Document) == 0 || p.UpdatedAt.IsZero() {
		err := errors.New("wsm: a merge progress record needs a workspace, a lease, a document and a time")
		s.log.Error(op, "refused an incomplete merge progress record", withError(fields, err))
		return err
	}
	return s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		_, err := tx.ExecContext(ctx,
			`INSERT INTO merge_progress (workspace_id, lease_id, updated_at, document) VALUES (?, ?, ?, ?)
			 ON CONFLICT(workspace_id) DO UPDATE SET
			   lease_id = excluded.lease_id, updated_at = excluded.updated_at, document = excluded.document`,
			p.Workspace, p.Lease, nanos(p.UpdatedAt), string(p.Document))
		return err
	})
}

// MergeProgressOf loads a workspace's merge progress record; the bool is false
// when none stands.
func (s *store) MergeProgressOf(ctx context.Context, id WorkspaceID) (MergeProgress, bool, error) {
	var (
		out   MergeProgress
		found bool
	)
	err := s.read(ctx, "daemon.wsm.merge_progress", dlog.Context{"workspace": string(id)}, func(ctx context.Context) error {
		var (
			lease    string
			updated  int64
			document string
		)
		err := s.db().QueryRowContext(ctx,
			`SELECT lease_id, updated_at, document FROM merge_progress WHERE workspace_id = ?`, id).
			Scan(&lease, &updated, &document)
		if errors.Is(err, sql.ErrNoRows) {
			out, found = MergeProgress{}, false
			return nil
		}
		if err != nil {
			return err
		}
		if lease == "" || document == "" {
			return &DecodeError{Table: "merge_progress", Row: string(id), Err: fmt.Errorf("a stored record names its lease and carries a document")}
		}
		out = MergeProgress{Workspace: id, Lease: LeaseID(lease), UpdatedAt: fromNanos(updated), Document: []byte(document)}
		found = true
		return nil
	})
	if err != nil {
		return MergeProgress{}, false, err
	}
	return out, found, nil
}

// DropMergeProgress deletes the progress record of one merge lease and
// reports whether one stood. A record of ANOTHER lease of the same workspace
// is left alone: it belongs to a merge this caller does not own.
func (s *store) DropMergeProgress(ctx context.Context, lease LeaseID) (bool, error) {
	const op = "daemon.wsm.drop_merge_progress"
	dropped := false
	err := s.write(ctx, op, dlog.Context{"lease": string(lease)}, func(ctx context.Context, tx *sql.Tx) error {
		res, err := tx.ExecContext(ctx, `DELETE FROM merge_progress WHERE lease_id = ?`, lease)
		if err != nil {
			return err
		}
		n, err := res.RowsAffected()
		if err != nil {
			return err
		}
		dropped = n > 0
		return nil
	})
	if err != nil {
		return false, err
	}
	return dropped, nil
}
