package wsm

import (
	"context"
	"errors"
	"fmt"
	"slices"
	"testing"
	"time"
)

func TestOpenMergeLedgerRecordsTheLease(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	lease := NewLeaseID()

	// Act
	if err := s.OpenMergeLedger(context.Background(), ws.ID, lease); err != nil {
		t.Fatalf("OpenMergeLedger: %v", err)
	}
	got, err := s.MergeLedger(context.Background(), ws.ID)

	// Assert
	if err != nil {
		t.Fatalf("MergeLedger: %v", err)
	}
	if len(got) != 1 || got[0].Lease != lease || got[0].Workspace != ws.ID {
		t.Fatalf("ledger = %+v, want one entry for lease %q", got, lease)
	}
}

func TestOpenMergeLedgerIsIdempotent(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	lease := NewLeaseID()
	if err := s.OpenMergeLedger(context.Background(), ws.ID, lease); err != nil {
		t.Fatalf("OpenMergeLedger: %v", err)
	}

	// Act
	if err := s.OpenMergeLedger(context.Background(), ws.ID, lease); err != nil {
		t.Fatalf("OpenMergeLedger again: %v", err)
	}

	// Assert
	got, err := s.MergeLedger(context.Background(), ws.ID)
	if err != nil {
		t.Fatalf("MergeLedger: %v", err)
	}
	if len(got) != 1 {
		t.Fatalf("ledger has %d entries, want 1", len(got))
	}
}

func TestOpenMergeLedgerRefusesAnUnknownWorkspace(t *testing.T) {
	// Arrange
	s, _ := testStore(t)

	// Act
	err := s.OpenMergeLedger(context.Background(), WorkspaceID("absent"), NewLeaseID())

	// Assert
	if err == nil {
		t.Fatalf("OpenMergeLedger on an unregistered workspace succeeded")
	}
}

func TestRecordTabIntervalRoundTripsARound(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	lease := NewLeaseID()
	if err := s.OpenMergeLedger(context.Background(), ws.ID, lease); err != nil {
		t.Fatalf("OpenMergeLedger: %v", err)
	}
	ended := instant.Add(time.Minute)
	interval := TabInterval{Round: 1, Kind: "conflicts", StartedAt: instant, EndedAt: &ended, Outcome: "resolved"}

	// Act
	if err := s.RecordTabInterval(context.Background(), lease, interval); err != nil {
		t.Fatalf("RecordTabInterval: %v", err)
	}
	got, err := s.MergeLedger(context.Background(), ws.ID)

	// Assert
	if err != nil {
		t.Fatalf("MergeLedger: %v", err)
	}
	if len(got[0].Intervals) != 1 {
		t.Fatalf("recorded %d intervals, want 1", len(got[0].Intervals))
	}
	restored := got[0].Intervals[0]
	if restored.Round != 1 || restored.Kind != "conflicts" || restored.Outcome != "resolved" {
		t.Fatalf("interval = %+v, want %+v", restored, interval)
	}
	if restored.EndedAt == nil || !restored.EndedAt.Equal(ended) {
		t.Fatalf("ended at = %v, want %v", restored.EndedAt, ended)
	}
}

func TestRecordTabIntervalKeepsRoundsInOrder(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	lease := NewLeaseID()
	if err := s.OpenMergeLedger(context.Background(), ws.ID, lease); err != nil {
		t.Fatalf("OpenMergeLedger: %v", err)
	}
	for _, round := range []int{3, 1, 2} {
		if err := s.RecordTabInterval(context.Background(), lease, TabInterval{Round: round, Kind: "tests", StartedAt: instant}); err != nil {
			t.Fatalf("RecordTabInterval: %v", err)
		}
	}

	// Act
	got, err := s.MergeLedger(context.Background(), ws.ID)

	// Assert
	if err != nil {
		t.Fatalf("MergeLedger: %v", err)
	}
	for i, want := range []int{1, 2, 3} {
		if got[0].Intervals[i].Round != want {
			t.Fatalf("interval %d round = %d, want %d", i, got[0].Intervals[i].Round, want)
		}
	}
}

func TestRecordTabIntervalClosesARunningRound(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	lease := NewLeaseID()
	if err := s.OpenMergeLedger(context.Background(), ws.ID, lease); err != nil {
		t.Fatalf("OpenMergeLedger: %v", err)
	}
	if err := s.RecordTabInterval(context.Background(), lease, TabInterval{Round: 1, Kind: "tests", StartedAt: instant}); err != nil {
		t.Fatalf("RecordTabInterval: %v", err)
	}
	ended := instant.Add(time.Minute)

	// Act
	if err := s.RecordTabInterval(context.Background(), lease, TabInterval{Round: 1, Kind: "tests", StartedAt: instant, EndedAt: &ended, Outcome: "passed"}); err != nil {
		t.Fatalf("RecordTabInterval: %v", err)
	}

	// Assert
	got, err := s.MergeLedger(context.Background(), ws.ID)
	if err != nil {
		t.Fatalf("MergeLedger: %v", err)
	}
	if len(got[0].Intervals) != 1 {
		t.Fatalf("recorded %d intervals, want the one round closed in place", len(got[0].Intervals))
	}
	if got[0].Intervals[0].Outcome != "passed" {
		t.Fatalf("outcome = %q, want %q", got[0].Intervals[0].Outcome, "passed")
	}
}

func TestRecordTabIntervalRefusesRoundZero(t *testing.T) {
	// Arrange
	s, log := testStore(t)
	ws := testWorkspace(t, s)
	lease := NewLeaseID()
	if err := s.OpenMergeLedger(context.Background(), ws.ID, lease); err != nil {
		t.Fatalf("OpenMergeLedger: %v", err)
	}

	// Act
	err := s.RecordTabInterval(context.Background(), lease, TabInterval{Round: 0, Kind: "tests", StartedAt: instant})

	// Assert
	if err == nil {
		t.Fatalf("RecordTabInterval with round 0 succeeded")
	}
	if !loggedOperation(log, "daemon.wsm.record_tab_interval", "error") {
		t.Fatalf("the refusal was not logged at error: %v", log.Records())
	}
}

func TestRecordTabIntervalRefusesAnEmptyKind(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	lease := NewLeaseID()
	if err := s.OpenMergeLedger(context.Background(), ws.ID, lease); err != nil {
		t.Fatalf("OpenMergeLedger: %v", err)
	}

	// Act
	err := s.RecordTabInterval(context.Background(), lease, TabInterval{Round: 1, StartedAt: instant})

	// Assert
	if err == nil {
		t.Fatalf("RecordTabInterval with no kind succeeded")
	}
}

func TestRecordTabIntervalRefusesAnUnopenedLedger(t *testing.T) {
	// Arrange
	s, _ := testStore(t)

	// Act
	err := s.RecordTabInterval(context.Background(), LeaseID("absent"), TabInterval{Round: 1, Kind: "tests", StartedAt: instant})

	// Assert
	if !errors.Is(err, ErrNotFound) {
		t.Fatalf("RecordTabInterval = %v, want ErrNotFound", err)
	}
}

func TestMergeLedgerFailsWholeOnACorruptRound(t *testing.T) {
	// Arrange
	s, log := testStore(t)
	ws := testWorkspace(t, s)
	lease := NewLeaseID()
	if err := s.OpenMergeLedger(context.Background(), ws.ID, lease); err != nil {
		t.Fatalf("OpenMergeLedger: %v", err)
	}
	if err := s.RecordTabInterval(context.Background(), lease, TabInterval{Round: 1, Kind: "tests", StartedAt: instant}); err != nil {
		t.Fatalf("RecordTabInterval: %v", err)
	}
	corrupt(t, s, `UPDATE merge_tab_intervals SET round = 0 WHERE lease_id = ?`, lease)

	// Act
	got, err := s.MergeLedger(context.Background(), ws.ID)

	// Assert
	var refusal *DecodeError
	if !errors.As(err, &refusal) || refusal.Table != "merge_tab_intervals" || refusal.Field != "round" {
		t.Fatalf("MergeLedger = %v, want a *DecodeError naming merge_tab_intervals.round", err)
	}
	if got != nil {
		t.Fatalf("loaded %d entries alongside the refusal, want none", len(got))
	}
	if !loggedOperation(log, "daemon.wsm.merge_ledger", "error") {
		t.Fatalf("the decode failure was not logged at error: %v", log.Records())
	}
}

func TestForgetDeletesAWorkspacesLedger(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	lease := NewLeaseID()
	if err := s.OpenMergeLedger(context.Background(), ws.ID, lease); err != nil {
		t.Fatalf("OpenMergeLedger: %v", err)
	}
	if err := s.RecordTabInterval(context.Background(), lease, TabInterval{Round: 1, Kind: "merge", StartedAt: instant}); err != nil {
		t.Fatalf("RecordTabInterval: %v", err)
	}

	// Act
	if _, err := s.Forget(context.Background(), ws.ID); err != nil {
		t.Fatalf("Forget: %v", err)
	}

	// Assert
	if n := scalar[int](t, s, `SELECT count(*) FROM merge_tab_intervals WHERE lease_id = ?`, lease); n != 0 {
		t.Fatalf("%d intervals survived the nuke, want none", n)
	}
}

func TestMergeLedgerWorkspace(t *testing.T) {
	tests := []struct {
		name     string
		known    bool
		wantErr  error
		wantOwns bool
	}{
		{name: "a lease a ledger holds answers its workspace", known: true, wantOwns: true},
		{name: "a lease no ledger holds is not found", known: false, wantErr: ErrNotFound},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			ws := testWorkspace(t, s)
			lease := NewLeaseID()
			if tt.known {
				if err := s.OpenMergeLedger(context.Background(), ws.ID, lease); err != nil {
					t.Fatalf("OpenMergeLedger: %v", err)
				}
			}

			// Act
			got, err := s.MergeLedgerWorkspace(context.Background(), lease)

			// Assert
			if tt.wantErr != nil {
				if !errors.Is(err, tt.wantErr) || got != "" {
					t.Fatalf("MergeLedgerWorkspace = (%q, %v), want %v", got, err, tt.wantErr)
				}
				return
			}
			if err != nil || got != ws.ID {
				t.Fatalf("MergeLedgerWorkspace = (%q, %v), want %q", got, err, ws.ID)
			}
		})
	}
}

func TestALedgerHoldsAtMostOneLiveRound(t *testing.T) {
	settledAt := instant.Add(time.Second)
	tests := []struct {
		name        string
		then        TabInterval
		wantLive    []string
		wantCutKind string
	}{
		{name: "a new live round settles the one left live", then: TabInterval{Round: 2, Kind: "rebasing", StartedAt: instant.Add(time.Minute)},
			wantLive: []string{"rebasing/2"}, wantCutKind: "rebasing/1"},
		{name: "rewriting the live round itself keeps it live", then: TabInterval{Round: 1, Kind: "rebasing", StartedAt: instant},
			wantLive: []string{"rebasing/1"}},
		{name: "a settled round written beside it leaves the live one live", then: TabInterval{Round: 1, Kind: "queue", StartedAt: instant, EndedAt: &settledAt, Outcome: "succeeded"},
			wantLive: []string{"rebasing/1"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange: one live rebasing round.
			s, _ := testStore(t)
			ws := testWorkspace(t, s)
			lease := NewLeaseID()
			ctx := context.Background()
			if err := s.OpenMergeLedger(ctx, ws.ID, lease); err != nil {
				t.Fatal(err)
			}
			if err := s.RecordTabInterval(ctx, lease, TabInterval{Round: 1, Kind: "rebasing", StartedAt: instant}); err != nil {
				t.Fatal(err)
			}

			// Act
			if err := s.RecordTabInterval(ctx, lease, tt.then); err != nil {
				t.Fatalf("RecordTabInterval: %v", err)
			}

			// Assert
			got, err := s.MergeLedger(ctx, ws.ID)
			if err != nil {
				t.Fatal(err)
			}
			var live []string
			for _, iv := range got[0].Intervals {
				key := fmt.Sprintf("%s/%d", iv.Kind, iv.Round)
				if iv.EndedAt == nil {
					live = append(live, key)
				}
				if key == tt.wantCutKind && (iv.Outcome != TabOutcomeRestarted || iv.EndedAt == nil || !iv.EndedAt.Equal(tt.then.StartedAt)) {
					t.Fatalf("cut round = %+v, want it settled restarted at the new round's start", iv)
				}
			}
			if !slices.Equal(live, tt.wantLive) {
				t.Fatalf("live rounds = %v, want %v", live, tt.wantLive)
			}
		})
	}
}

func TestTheSettledLiveRoundsStepLeavesOnlyEachQueuedMergesCurrentRoundLive(t *testing.T) {
	// Arrange: a queued merge's ledger with two rounds left live and its
	// current one, and an old ledger of the same workspace with a live round.
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	repo := RepoKey(t.TempDir())
	current, old := LeaseID("ledger-current"), LeaseID("ledger-old")
	queued(t, s, repo, ws.ID, instant)
	for _, stmt := range []string{
		`UPDATE merge_queue SET ledger_id = 'ledger-current'`,
		`INSERT INTO merge_ledger (lease_id, workspace_id, opened_at) VALUES ('ledger-current', '` + string(ws.ID) + `', 1), ('ledger-old', '` + string(ws.ID) + `', 1)`,
		`INSERT INTO merge_tab_intervals (lease_id, round, kind, started_at, outcome) VALUES ('ledger-current', 1, 'rebasing', 100, ''), ('ledger-current', 2, 'rebasing', 200, ''), ('ledger-current', 1, 'tests', 300, ''), ('ledger-old', 1, 'rebasing', 50, '')`,
	} {
		if _, err := s.db().Exec(stmt); err != nil {
			t.Fatalf("%s: %v", stmt, err)
		}
	}

	// Act
	if _, err := s.db().Exec(settledLiveRoundsDDL); err != nil {
		t.Fatalf("the layout-152 step: %v", err)
	}

	// Assert
	want := map[string]struct {
		ended   int64
		outcome string
	}{
		"ledger-current/rebasing/1": {200, "restarted"},
		"ledger-current/rebasing/2": {300, "restarted"},
		"ledger-current/tests/1":    {0, ""},
		"ledger-old/rebasing/1":     {50, "restarted"},
	}
	for _, lease := range []LeaseID{current, old} {
		rows, err := s.db().Query(`SELECT round, kind, COALESCE(ended_at, 0), outcome FROM merge_tab_intervals WHERE lease_id = ?`, lease)
		if err != nil {
			t.Fatal(err)
		}
		for rows.Next() {
			var round int
			var kind, outcome string
			var ended int64
			if err := rows.Scan(&round, &kind, &ended, &outcome); err != nil {
				t.Fatal(err)
			}
			key := fmt.Sprintf("%s/%s/%d", lease, kind, round)
			if w := want[key]; w.ended != ended || w.outcome != outcome {
				t.Errorf("%s = ended %d %q, want %d %q", key, ended, outcome, w.ended, w.outcome)
			}
		}
		rows.Close()
	}
}
