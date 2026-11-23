package wsm

import (
	"context"
	"database/sql"
	"fmt"

	"agentrepl/logging/logsite"
	frontendv1 "agentrepl/proto/frontend/v1"

	"google.golang.org/protobuf/proto"

	"agent-repld/internal/dlog"
	"agent-repld/internal/feedid"
)

// LAYOUT 155 FOLDS A QUEUED MERGE'S EXTRA LEDGERS INTO ITS ONE. Before every
// step read its queue entry's ledger (merge's ledgerOfEntry), a waiting
// merge's background rebase or an admission after a restart minted a second
// ledger, and the merge was drawn in two bubbles (2026-11-15:
// resizable-sidebar-sections). Each ledger of a queued merge's workspace
// opened since it joined the line, other than its entry's, is folded into
// the entry's: its rounds are renumbered after the rounds of each kind the
// entry's ledger holds, at most one round is left live (the latest), and the
// extra ledger goes, its bubble's stored rows with it. A ledger a lease still
// holds belongs to a run in flight and is left alone.

// foldedLedger is one extra ledger and the ledger it folds into.
type foldedLedger struct {
	from, into LeaseID
}

// foldExtraMergeLedgers is layout 158's rewrite.
func foldExtraMergeLedgers(ctx context.Context, tx *sql.Tx, log dlog.Logger) error {
	rows, err := tx.QueryContext(ctx, `
		SELECT l.lease_id, q.ledger_id FROM merge_queue AS q
		JOIN merge_ledger AS l ON l.workspace_id = q.workspace_id
		WHERE q.ledger_id != '' AND l.lease_id != q.ledger_id AND l.opened_at >= q.enqueued_at
		  AND NOT EXISTS (SELECT 1 FROM leases WHERE leases.id = l.lease_id)
		ORDER BY q.ledger_id, l.opened_at, l.lease_id`)
	if err != nil {
		return err
	}
	var folds []foldedLedger
	for rows.Next() {
		var f foldedLedger
		if err := rows.Scan(&f.from, &f.into); err != nil {
			rows.Close()
			return err
		}
		folds = append(folds, f)
	}
	if err := rows.Close(); err != nil {
		return err
	}
	if len(folds) == 0 {
		return nil
	}
	gone := map[LeaseID]bool{}
	for _, f := range folds {
		if err := foldLedger(ctx, tx, f); err != nil {
			return fmt.Errorf("wsm: fold ledger %s into %s: %w", f.from, f.into, err)
		}
		gone[f.from] = true
	}
	// AT MOST ONE ROUND OF A LEDGER STAYS LIVE: the latest (layout 155's rule).
	if _, err := settleLiveRounds(ctx, tx, TabOutcomeRestarted,
		sqlExpr{`(SELECT MIN(later.started_at) FROM merge_tab_intervals AS later
		              WHERE later.lease_id = merge_tab_intervals.lease_id AND later.started_at > merge_tab_intervals.started_at)`, nil},
		sqlExpr{`EXISTS (SELECT 1 FROM merge_tab_intervals AS later
		  WHERE later.lease_id = merge_tab_intervals.lease_id AND later.started_at > merge_tab_intervals.started_at)`, nil}); err != nil {
		return err
	}
	log.Info("daemon.wsm.migrate", "folded each queued merge's extra ledgers into its entry's", dlog.Context{"folded": len(folds)})
	return dropDurableRows(ctx, tx, log, logsite.Msg("dropped the bubbles of the folded ledgers"), func(raw []byte) (bool, error) {
		return isFoldedBubbleRow(raw, gone)
	})
}

// foldLedger moves F's rounds into its target, each renumbered after the
// rounds of its kind the target holds, then deletes F.
func foldLedger(ctx context.Context, tx *sql.Tx, f foldedLedger) error {
	rows, err := tx.QueryContext(ctx, `SELECT round, kind FROM merge_tab_intervals WHERE lease_id = ? ORDER BY started_at, round, kind`, f.from)
	if err != nil {
		return err
	}
	type key struct {
		round int
		kind  string
	}
	var rounds []key
	for rows.Next() {
		var k key
		if err := rows.Scan(&k.round, &k.kind); err != nil {
			rows.Close()
			return err
		}
		rounds = append(rounds, k)
	}
	if err := rows.Close(); err != nil {
		return err
	}
	for _, k := range rounds {
		var last sql.NullInt64
		if err := tx.QueryRowContext(ctx, `SELECT MAX(round) FROM merge_tab_intervals WHERE lease_id = ? AND kind = ?`, f.into, k.kind).Scan(&last); err != nil {
			return err
		}
		if _, err := tx.ExecContext(ctx, `
			INSERT INTO merge_tab_intervals (lease_id, round, kind, started_at, ended_at, outcome, held_since)
			SELECT ?, ?, kind, started_at, ended_at, outcome, held_since FROM merge_tab_intervals
			WHERE lease_id = ? AND round = ? AND kind = ?`,
			f.into, last.Int64+1, f.from, k.round, k.kind); err != nil {
			return err
		}
	}
	for _, stmt := range []string{
		`DELETE FROM merge_tab_intervals WHERE lease_id = ?`,
		`DELETE FROM merge_progress WHERE lease_id = ?`,
		`DELETE FROM merge_ledger WHERE lease_id = ?`,
	} {
		if _, err := tx.ExecContext(ctx, stmt, f.from); err != nil {
			return err
		}
	}
	return nil
}

// isFoldedBubbleRow reports whether RAW is a row of a bubble whose ledger was
// folded away: its head on the root feed, or a row of its sub-feed.
func isFoldedBubbleRow(raw []byte, gone map[LeaseID]bool) (bool, error) {
	row := &frontendv1.FeedRow{}
	if err := proto.Unmarshal(raw, row); err != nil {
		return false, err
	}
	ref, err := feedid.Decode(row.GetId())
	if err != nil {
		return false, nil
	}
	if ref.Feed.Merge != nil && gone[LeaseID(*ref.Feed.Merge)] {
		return true, nil
	}
	return ref.Row.Kind == feedid.KindMergeHead && gone[LeaseID(ref.Row.ID)], nil
}
