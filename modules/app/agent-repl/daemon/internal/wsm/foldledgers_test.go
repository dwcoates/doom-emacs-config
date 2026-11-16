package wsm

import (
	"context"
	"database/sql"
	"slices"
	"testing"
	"time"

	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/feedid"
	"agent-repld/internal/ids"
)

// storedBubbleRow writes one durable row of REF's bubble for WS.
func storedBubbleRow(t *testing.T, s *store, ws WorkspaceID, ref feedid.Ref) {
	t.Helper()
	row := &frontendv1.FeedRow{Id: feedid.Encode(ref), Row: &frontendv1.FeedRow_MergeTab{MergeTab: &frontendv1.FeedMergeTab{}}}
	if _, err := s.db().Exec(`INSERT INTO durable_feed_rows (workspace_id, row_id, plane, order_key, row) VALUES (?, ?, 0, 'k', ?)`,
		ws, feedid.Encode(ref).GetValue(), marshalled(t, row)); err != nil {
		t.Fatalf("insert row: %v", err)
	}
}

// roundsOf answers LEASE's rounds as "kind/round" in start order.
func roundsOf(t *testing.T, s *store, lease LeaseID) []string {
	t.Helper()
	intervals, err := s.tabIntervals(context.Background(), lease)
	if err != nil {
		t.Fatalf("tabIntervals: %v", err)
	}
	var out []string
	for _, r := range intervals {
		out = append(out, r.Kind+"/"+string(rune('0'+r.Round)))
	}
	return out
}

func TestTheFoldMovesAQueuedMergesExtraLedgerIntoItsEntrysAndDropsItsBubble(t *testing.T) {
	// Arrange: a queued merge, its entry's ledger with one round, a second
	// ledger a background rebase opened since, and an earlier merge's ledger.
	s, _ := testStore(t)
	ctx := context.Background()
	repo := RepoKey(t.TempDir())
	ws := testWorkspace(t, s).ID
	into, extra, earlier := NewLeaseID(), NewLeaseID(), NewLeaseID()
	if err := s.RequestMerge(ctx, repo, ws, ownBranch, instant); err != nil {
		t.Fatal(err)
	}
	if _, err := s.QueueMerge(ctx, repo, ws, into); err != nil {
		t.Fatal(err)
	}
	for _, lease := range []LeaseID{into, extra, earlier} {
		if err := s.OpenMergeLedger(ctx, ws, lease); err != nil {
			t.Fatal(err)
		}
	}
	if _, err := s.db().Exec(`UPDATE merge_ledger SET opened_at = 0 WHERE lease_id = ?`, earlier); err != nil {
		t.Fatal(err)
	}
	failed := instant.Add(2 * time.Second)
	record := func(lease LeaseID, r TabInterval) {
		if err := s.RecordTabInterval(ctx, lease, r); err != nil {
			t.Fatal(err)
		}
	}
	record(into, TabInterval{Round: 1, Kind: "rebasing", StartedAt: instant.Add(time.Second), EndedAt: &failed, Outcome: "failed"})
	settled := instant.Add(4 * time.Second)
	record(extra, TabInterval{Round: 1, Kind: "rebasing", StartedAt: instant.Add(3 * time.Second), EndedAt: &settled, Outcome: "failed"})
	record(extra, TabInterval{Round: 1, Kind: "conflicts", StartedAt: instant.Add(5 * time.Second)})
	record(earlier, TabInterval{Round: 1, Kind: "tests", StartedAt: instant.Add(-time.Hour), EndedAt: &failed, Outcome: "succeeded"})
	extraID, intoID := ids.LeaseID(extra), ids.LeaseID(into)
	storedBubbleRow(t, s, ws, feedid.Ref{WS: ws, Feed: feedid.Feed{Root: true}, Row: feedid.RowKey{Kind: feedid.KindMergeHead, ID: string(extra)}})
	storedBubbleRow(t, s, ws, feedid.Ref{WS: ws, Feed: feedid.Feed{Merge: &extraID}, Row: feedid.RowKey{Kind: feedid.KindMergeTab, ID: "rebasing", Sub: "1"}})
	storedBubbleRow(t, s, ws, feedid.Ref{WS: ws, Feed: feedid.Feed{Merge: &intoID}, Row: feedid.RowKey{Kind: feedid.KindMergeTab, ID: "rebasing", Sub: "1"}})

	// Act
	err := s.write(ctx, "test", dlog.Context{}, func(ctx context.Context, tx *sql.Tx) error {
		return foldExtraMergeLedgers(ctx, tx, s.log)
	})

	// Assert: the rounds folded, renumbered after the entry's own.
	if err != nil {
		t.Fatalf("foldExtraMergeLedgers: %v", err)
	}
	got := roundsOf(t, s, into)
	slices.Sort(got)
	if !slices.Equal(got, []string{"conflicts/1", "rebasing/1", "rebasing/2"}) {
		t.Fatalf("the entry's rounds = %v, want rebasing 1 and 2 and conflicts 1", got)
	}
	if n := scalar[int](t, s, `SELECT COUNT(*) FROM merge_ledger WHERE lease_id = ?`, extra); n != 0 {
		t.Fatal("the extra ledger survived the fold")
	}
	if n := scalar[int](t, s, `SELECT COUNT(*) FROM merge_ledger WHERE lease_id = ?`, earlier); n != 1 {
		t.Fatal("an earlier merge's ledger was folded")
	}
	if n := scalar[int](t, s, `SELECT COUNT(*) FROM durable_feed_rows WHERE workspace_id = ?`, ws); n != 1 {
		t.Fatalf("stored rows = %d, want only the entry's bubble's row", n)
	}
}

func TestTheFoldLeavesALedgerALeaseStillHolds(t *testing.T) {
	// Arrange: the extra ledger is a run's in flight.
	s, _ := testStore(t)
	ctx := context.Background()
	repo := RepoKey(t.TempDir())
	ws := testWorkspace(t, s).ID
	into := NewLeaseID()
	if err := s.RequestMerge(ctx, repo, ws, ownBranch, instant); err != nil {
		t.Fatal(err)
	}
	if _, err := s.QueueMerge(ctx, repo, ws, into); err != nil {
		t.Fatal(err)
	}
	lease, err := s.AcquireLease(ctx, ws, HolderMerge, PolicyHold)
	if err != nil {
		t.Fatal(err)
	}
	if err := s.OpenMergeLedger(ctx, ws, lease.ID); err != nil {
		t.Fatal(err)
	}

	// Act
	err = s.write(ctx, "test", dlog.Context{}, func(ctx context.Context, tx *sql.Tx) error {
		return foldExtraMergeLedgers(ctx, tx, s.log)
	})

	// Assert
	if n := scalar[int](t, s, `SELECT COUNT(*) FROM merge_ledger WHERE lease_id = ?`, lease.ID); err != nil || n != 1 {
		t.Fatalf("fold = %v, the held ledger has %d rows, want it left", err, n)
	}
}
