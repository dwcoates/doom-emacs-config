package wsm

import (
	"context"
	"database/sql"
	"time"

	"agent-repld/internal/dlog"
)

// This file is the durable half of HANDING A LANDING BACK ONLY ONCE IT IS
// DEPLOYED: the report an async merge owes its requester when it landed in the
// daemon's own checkout, held until the build that carries it is running.
//
// THE ROW IS THE DEBT, AND DELETING IT IS THE PAYMENT. The merge owes the
// report at its landing, BEFORE it tells the deploy, so every deploy that
// starts after the landing finds the row. The deploy COVERS every row present
// when it commits to building the checkout (CoverOwedHandBacks), because what
// it builds carries every landing before that instant. Whoever finishes that
// deploy -- the daemon that stays, or the successor it hands over to -- pays
// the covered rows. A row a deploy never covered waits for the next deploy.
//
// THE PAYMENT IS EXACTLY ONCE. Each row carries the idempotency key minted
// when it was owed, so a payer that submitted the report and died before
// deleting the row submits the same key again, which the prompt handler
// answers with the turn it already made.

// owedHandBacksDDL adds owed_handbacks: one row per landed async merge whose
// report waits for its deploy.
//
// The fresh-file schema appends this same text (schema.go), so a migrated file
// and a created one cannot differ.
const owedHandBacksDDL = `
CREATE TABLE owed_handbacks (
  merge_workspace TEXT PRIMARY KEY,
  requester       TEXT NOT NULL,
  merge_name      TEXT NOT NULL,
  branch          TEXT NOT NULL,
  summary         TEXT NOT NULL,
  compact_after   INTEGER NOT NULL,
  idempotency_key TEXT NOT NULL,
  owed_at         INTEGER NOT NULL,
  covered         INTEGER NOT NULL DEFAULT 0
);
`

// OwedHandBack is one landed async merge's report, owed to its requester until
// the build that carries the landing is running.
type OwedHandBack struct {
	// MergeWorkspace is the merge workspace the merge ran in; it names the debt.
	MergeWorkspace WorkspaceID
	// Requester is the workspace the report goes to.
	Requester WorkspaceID
	// MergeName is the merge workspace's name, which the report quotes.
	MergeName string
	// Branch is what the report's badge says was merged.
	Branch string
	// Summary is how the merge landed ("it landed as commit ...").
	Summary string
	// CompactAfter is whether the requester asked to be compacted once the
	// report is delivered.
	CompactAfter bool
	// IdempotencyKey is the report's submission key, minted when it was owed.
	IdempotencyKey string
	// OwedAt is when the merge landed.
	OwedAt time.Time
	// Covered is whether a deploy committed to a build that carries it.
	Covered bool
}

// OweHandBack records a landed merge's report as owed. Owing the same merge
// workspace twice keeps the first row.
func (s *store) OweHandBack(ctx context.Context, owed OwedHandBack) error {
	const op = "daemon.wsm.owe_handback"
	fields := dlog.Context{"merge_workspace": string(owed.MergeWorkspace), "requester": string(owed.Requester), "owed_at": owed.OwedAt}
	return s.write(ctx, op, fields, func(ctx context.Context, tx *sql.Tx) error {
		_, err := tx.ExecContext(ctx,
			`INSERT INTO owed_handbacks (merge_workspace, requester, merge_name, branch, summary, compact_after, idempotency_key, owed_at)
			 VALUES (?, ?, ?, ?, ?, ?, ?, ?) ON CONFLICT(merge_workspace) DO NOTHING`,
			owed.MergeWorkspace, owed.Requester, owed.MergeName, owed.Branch, owed.Summary, owed.CompactAfter, owed.IdempotencyKey, nanos(owed.OwedAt))
		return err
	})
}

// CoverOwedHandBacks marks every owed report as covered by the deploy that is
// committing now, and answers how many it newly covered.
func (s *store) CoverOwedHandBacks(ctx context.Context) (int, error) {
	const op = "daemon.wsm.cover_handbacks"
	var covered int
	err := s.write(ctx, op, nil, func(ctx context.Context, tx *sql.Tx) error {
		res, err := tx.ExecContext(ctx, `UPDATE owed_handbacks SET covered = 1 WHERE covered = 0`)
		if err != nil {
			return err
		}
		n, err := res.RowsAffected()
		if err != nil {
			return err
		}
		covered = int(n)
		return nil
	})
	if err != nil {
		return 0, err
	}
	return covered, nil
}

// OwedHandBacks answers the owed reports, oldest first: only the covered ones
// when coveredOnly, else every one.
func (s *store) OwedHandBacks(ctx context.Context, coveredOnly bool) ([]OwedHandBack, error) {
	const op = "daemon.wsm.owed_handbacks"
	var out []OwedHandBack
	err := s.read(ctx, op, dlog.Context{"covered_only": coveredOnly}, func(ctx context.Context) error {
		rows, err := s.db().QueryContext(ctx,
			`SELECT merge_workspace, requester, merge_name, branch, summary, compact_after, idempotency_key, owed_at, covered
			 FROM owed_handbacks WHERE covered = 1 OR NOT ? ORDER BY owed_at, merge_workspace`, coveredOnly)
		if err != nil {
			return err
		}
		defer rows.Close()
		for rows.Next() {
			var (
				owed   OwedHandBack
				owedAt int64
			)
			if err := rows.Scan(&owed.MergeWorkspace, &owed.Requester, &owed.MergeName, &owed.Branch, &owed.Summary,
				&owed.CompactAfter, &owed.IdempotencyKey, &owedAt, &owed.Covered); err != nil {
				return err
			}
			owed.OwedAt = fromNanos(owedAt)
			out = append(out, owed)
		}
		return rows.Err()
	})
	if err != nil {
		return nil, err
	}
	return out, nil
}

// PayHandBack deletes the owed report of mergeWS, once it was submitted. It
// answers whether a row was there to delete.
func (s *store) PayHandBack(ctx context.Context, mergeWS WorkspaceID) (bool, error) {
	const op = "daemon.wsm.pay_handback"
	var paid bool
	err := s.write(ctx, op, dlog.Context{"merge_workspace": string(mergeWS)}, func(ctx context.Context, tx *sql.Tx) error {
		res, err := tx.ExecContext(ctx, `DELETE FROM owed_handbacks WHERE merge_workspace = ?`, mergeWS)
		if err != nil {
			return err
		}
		n, err := res.RowsAffected()
		if err != nil {
			return err
		}
		paid = n > 0
		return nil
	})
	return paid, err
}
