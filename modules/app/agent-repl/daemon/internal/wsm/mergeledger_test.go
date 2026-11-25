package wsm

import (
	"context"
	"errors"
	"fmt"
	"slices"
	"testing"
	"time"

	"agent-repld/internal/dlog"
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
		t.Fatalf("the layout-155 step: %v", err)
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

// liveRound opens a ledger for a workspace with one live tests round and
// answers the workspace and the ledger.
func liveRound(t *testing.T, s *store) (WorkspaceID, LeaseID) {
	t.Helper()
	ws := testWorkspace(t, s)
	lease := NewLeaseID()
	if err := s.OpenMergeLedger(context.Background(), ws.ID, lease); err != nil {
		t.Fatalf("OpenMergeLedger: %v", err)
	}
	if err := s.RecordTabInterval(context.Background(), lease, TabInterval{Round: 1, Kind: "tests", StartedAt: instant}); err != nil {
		t.Fatalf("RecordTabInterval: %v", err)
	}
	return ws.ID, lease
}

// onlyRound reads WS's one round.
func onlyRound(t *testing.T, s *store, ws WorkspaceID) TabInterval {
	t.Helper()
	got, err := s.MergeLedger(context.Background(), ws)
	if err != nil || len(got) != 1 || len(got[0].Intervals) != 1 {
		t.Fatalf("MergeLedger = (%+v, %v), want one ledger with one round", got, err)
	}
	return got[0].Intervals[0]
}

func TestHoldMergeRoundMarksAndClearsTheLiveRound(t *testing.T) {
	held := instant.Add(time.Minute)
	tests := []struct {
		name     string
		hold     []*time.Time
		wantHeld *time.Time
	}{
		{name: "a hold marks the live round held since its instant", hold: []*time.Time{&held}, wantHeld: &held},
		{name: "a release clears the hold", hold: []*time.Time{&held, nil}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			ws, lease := liveRound(t, s)

			// Act
			for _, at := range tt.hold {
				if ok, err := s.HoldMergeRound(context.Background(), lease, at); err != nil || !ok {
					t.Fatalf("HoldMergeRound = (%v, %v), want the live round marked", ok, err)
				}
			}

			// Assert
			got := onlyRound(t, s, ws)
			if (got.HeldSince == nil) != (tt.wantHeld == nil) || (got.HeldSince != nil && !got.HeldSince.Equal(*tt.wantHeld)) {
				t.Fatalf("held since %v, want %v", got.HeldSince, tt.wantHeld)
			}
		})
	}
}

func TestSettlingAHeldRoundClearsItsHold(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws, lease := liveRound(t, s)
	held := instant.Add(time.Minute)
	if _, err := s.HoldMergeRound(context.Background(), lease, &held); err != nil {
		t.Fatal(err)
	}
	ended := instant.Add(2 * time.Minute)

	// Act
	err := s.RecordTabInterval(context.Background(), lease, TabInterval{Round: 1, Kind: "tests", StartedAt: instant, EndedAt: &ended, Outcome: "passed"})

	// Assert
	if got := onlyRound(t, s, ws); err != nil || got.HeldSince != nil || got.EndedAt == nil {
		t.Fatalf("RecordTabInterval = %v, round %+v; want it settled and no longer held", err, got)
	}
}

func TestHoldMergeRoundOfALedgerWithNoLiveRoundMarksNothing(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws, lease := liveRound(t, s)
	ended := instant.Add(time.Minute)
	if err := s.RecordTabInterval(context.Background(), lease, TabInterval{Round: 1, Kind: "tests", StartedAt: instant, EndedAt: &ended, Outcome: "passed"}); err != nil {
		t.Fatal(err)
	}
	held := instant.Add(2 * time.Minute)

	// Act
	ok, err := s.HoldMergeRound(context.Background(), lease, &held)

	// Assert
	if err != nil || ok || onlyRound(t, s, ws).HeldSince != nil {
		t.Fatalf("HoldMergeRound = (%v, %v), want nothing marked", ok, err)
	}
}

func TestHoldMergeRoundRefusesALedgerItDoesNotName(t *testing.T) {
	// Arrange
	s, logs := testStore(t)
	held := instant

	// Act
	_, err := s.HoldMergeRound(context.Background(), "", &held)

	// Assert
	if err == nil {
		t.Fatal("HoldMergeRound accepted a hold naming no ledger")
	}
	if _, ok := dlog.FindRecord(logs.Records(), dlog.RecordQuery{Level: dlog.LevelError, Operation: "daemon.wsm.hold_merge_round"}); !ok {
		t.Fatalf("records = %+v, want the refusal at ERROR", logs.Records())
	}
}

func TestHeldMergeRoundsAnswersEveryHeldLiveRound(t *testing.T) {
	// Arrange: one held ledger and one running.
	s, _ := testStore(t)
	_, heldLease := liveRound(t, s)
	_, _ = liveRound(t, s)
	held := instant.Add(time.Minute)
	if _, err := s.HoldMergeRound(context.Background(), heldLease, &held); err != nil {
		t.Fatal(err)
	}

	// Act
	got, err := s.HeldMergeRounds(context.Background())

	// Assert
	if err != nil || len(got) != 1 || !got[heldLease].Equal(held) {
		t.Fatalf("HeldMergeRounds = (%v, %v), want only %s held since %v", got, err, heldLease, held)
	}
}

func TestTheUnheldSettledRoundsStepClearsOnlyASettledRoundsHold(t *testing.T) {
	// Arrange: one settled round left held, and one live round held.
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	dropSettledUnheldTriggers(t, s)
	for _, stmt := range []string{
		`INSERT INTO merge_ledger (lease_id, workspace_id, opened_at) VALUES ('ledger-a', '` + string(ws.ID) + `', 1)`,
		`INSERT INTO merge_tab_intervals (lease_id, round, kind, started_at, ended_at, outcome, held_since) VALUES ('ledger-a', 1, 'fixes', 100, 200, 'abandoned', 150), ('ledger-a', 1, 'tests', 300, NULL, '', 350)`,
	} {
		if _, err := s.db().Exec(stmt); err != nil {
			t.Fatalf("%s: %v", stmt, err)
		}
	}

	// Act
	if _, err := s.db().Exec(unheldSettledRoundsDDL); err != nil {
		t.Fatalf("the layout-170 step: %v", err)
	}

	// Assert
	want := map[string]int64{"fixes": 0, "tests": 350}
	for kind, held := range want {
		var got int64
		if err := s.db().QueryRow(`SELECT COALESCE(held_since, 0) FROM merge_tab_intervals WHERE lease_id = 'ledger-a' AND kind = ?`, kind).Scan(&got); err != nil {
			t.Fatal(err)
		}
		if got != held {
			t.Fatalf("%s held_since = %d, want %d", kind, got, held)
		}
	}
}

// heldRoundOf opens LEASE's ledger for WS with one live fixes round, held
// since the instant.
func heldRoundOf(t *testing.T, s *store, ws WorkspaceID, lease LeaseID) {
	t.Helper()
	ctx := context.Background()
	if err := s.OpenMergeLedger(ctx, ws, lease); err != nil {
		t.Fatalf("OpenMergeLedger: %v", err)
	}
	if err := s.RecordTabInterval(ctx, lease, TabInterval{Round: 3, Kind: "fixes", StartedAt: instant}); err != nil {
		t.Fatalf("RecordTabInterval: %v", err)
	}
	at := instant.Add(time.Minute)
	if marked, err := s.HoldMergeRound(ctx, lease, &at); err != nil || !marked {
		t.Fatalf("HoldMergeRound = %v, %v; want the live round held", marked, err)
	}
}

// dropSettledUnheldTriggers removes layout 170's triggers, so a test can
// store the row a writer before them could leave.
func dropSettledUnheldTriggers(t *testing.T, s *store) {
	t.Helper()
	for _, name := range []string{"merge_tab_intervals_settled_unheld_insert", "merge_tab_intervals_settled_unheld_update"} {
		if _, err := s.db().Exec(`DROP TRIGGER ` + name); err != nil {
			t.Fatalf("drop trigger %s: %v", name, err)
		}
	}
}

func TestTheStoreRefusesToLeaveARoundSettledAndHeld(t *testing.T) {
	tests := []struct {
		name string
		stmt string
	}{
		{name: "settling a held round without clearing its hold", stmt: `UPDATE merge_tab_intervals SET ended_at = 1, outcome = 'abandoned' WHERE lease_id = ?`},
		{name: "holding a settled round", stmt: `UPDATE merge_tab_intervals SET ended_at = 1, held_since = 2 WHERE lease_id = ?`},
		{name: "inserting a round settled and held", stmt: `INSERT INTO merge_tab_intervals (lease_id, round, kind, started_at, ended_at, outcome, held_since) VALUES (?, 4, 'fixes', 1, 2, 'done', 3)`},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			ws := testWorkspace(t, s)
			lease := NewLeaseID()
			heldRoundOf(t, s, ws.ID, lease)

			// Act
			_, err := s.db().Exec(tt.stmt, lease)

			// Assert
			if err == nil {
				t.Fatal("the statement stored a round settled and held, want it aborted")
			}
			if n := scalar[int](t, s, `SELECT COUNT(*) FROM merge_tab_intervals WHERE ended_at IS NOT NULL AND held_since IS NOT NULL`); n != 0 {
				t.Fatalf("%d rounds stand settled and held, want none", n)
			}
		})
	}
}

func TestTheLedgerReadStillRefusesARoundSettledAndHeld(t *testing.T) {
	// Arrange: the row a writer before the triggers left.
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	lease := NewLeaseID()
	heldRoundOf(t, s, ws.ID, lease)
	dropSettledUnheldTriggers(t, s)
	if _, err := s.db().Exec(`UPDATE merge_tab_intervals SET ended_at = ?, outcome = 'abandoned' WHERE lease_id = ?`, nanos(instant.Add(time.Hour)), lease); err != nil {
		t.Fatal(err)
	}

	// Act
	_, err := s.MergeLedger(context.Background(), ws.ID)

	// Assert
	var decode *DecodeError
	if !errors.As(err, &decode) || decode.Field != "held_since" || decode.Row != string(lease) {
		t.Fatalf("MergeLedger = %v, want the settled and held round refused on held_since", err)
	}
}

func TestTheMigrationRepairsAStoredSettledAndHeldRoundThenRefusesAnother(t *testing.T) {
	// Arrange: a layout-169 file holding the row the orphan sweep left.
	path := fixtureAt(t, 169)
	lease := NewLeaseID()
	execRaw(t, path, `INSERT INTO merge_ledger (lease_id, workspace_id, opened_at) VALUES (?, ?, 0)`, lease, fixtureWorkspaceID)
	execRaw(t, path, `INSERT INTO merge_tab_intervals (lease_id, round, kind, started_at, ended_at, outcome, held_since) VALUES (?, 3, 'fixes', 1, 3, 'abandoned', 2)`, lease)

	// Act
	handle, err := Open(context.Background(), path, WithUnsyncedWrites())
	if err != nil {
		t.Fatalf("Open on a layout-169 database: %v", err)
	}
	defer handle.Close()

	// Assert
	ledgers, err := handle.MergeLedger(context.Background(), fixtureWorkspaceID)
	if err != nil || len(ledgers) != 1 || ledgers[0].Intervals[0].HeldSince != nil || ledgers[0].Intervals[0].EndedAt == nil {
		t.Fatalf("MergeLedger = %+v, %v; want the round settled with no hold", ledgers, err)
	}
	if _, err := handle.(*store).db().Exec(`UPDATE merge_tab_intervals SET held_since = 4 WHERE lease_id = ?`, lease); err == nil {
		t.Fatal("the migrated file took a hold on a settled round, want layout 174's trigger to refuse it")
	}
}

func TestSettleLiveRounds(t *testing.T) {
	// Each case seeds one ledger's rounds as (kind, ended_at, held_since), 0
	// meaning NULL, and settles the live rounds WHERE picks.
	type round struct {
		kind    string
		ended   int64
		held    int64
		outcome string
	}
	tests := []struct {
		name      string
		seed      []round
		where     sqlExpr
		want      []round
		wantCount int64
	}{
		{
			name:      "a held live round is settled and its hold cleared",
			seed:      []round{{kind: "conflicts", held: 150}},
			where:     sqlExpr{`1`, nil},
			want:      []round{{kind: "conflicts", ended: 900, outcome: "abandoned"}},
			wantCount: 1,
		},
		{
			name:      "a running live round is settled",
			seed:      []round{{kind: "tests"}},
			where:     sqlExpr{`1`, nil},
			want:      []round{{kind: "tests", ended: 900, outcome: "abandoned"}},
			wantCount: 1,
		},
		{
			name:      "a settled round is left as it was settled",
			seed:      []round{{kind: "tests", ended: 300, outcome: "passed"}},
			where:     sqlExpr{`1`, nil},
			want:      []round{{kind: "tests", ended: 300, outcome: "passed"}},
			wantCount: 0,
		},
		{
			name:      "a live round WHERE does not pick keeps running and held",
			seed:      []round{{kind: "conflicts", held: 150}, {kind: "tests"}},
			where:     sqlExpr{`kind = ?`, []any{"tests"}},
			want:      []round{{kind: "conflicts", held: 150}, {kind: "tests", ended: 900, outcome: "abandoned"}},
			wantCount: 1,
		},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			ws := testWorkspace(t, s)
			corrupt(t, s, `INSERT INTO merge_ledger (lease_id, workspace_id, opened_at) VALUES ('ledger-a', ?, 1)`, ws.ID)
			for _, r := range tt.seed {
				corrupt(t, s, `INSERT INTO merge_tab_intervals (lease_id, round, kind, started_at, ended_at, outcome, held_since) VALUES ('ledger-a', 1, ?, 100, NULLIF(?, 0), ?, NULLIF(?, 0))`, r.kind, r.ended, r.outcome, r.held)
			}
			tx, err := s.db().BeginTx(context.Background(), nil)
			if err != nil {
				t.Fatal(err)
			}

			// Act
			got, err := settleLiveRounds(context.Background(), tx, "abandoned", sqlExpr{`?`, []any{900}}, tt.where)
			if cerr := tx.Commit(); err == nil {
				err = cerr
			}

			// Assert
			if err != nil || got != tt.wantCount {
				t.Fatalf("settleLiveRounds = (%d, %v), want %d settled", got, err, tt.wantCount)
			}
			for _, w := range tt.want {
				var r round
				if err := s.db().QueryRow(`SELECT kind, COALESCE(ended_at, 0), COALESCE(held_since, 0), outcome FROM merge_tab_intervals WHERE lease_id = 'ledger-a' AND kind = ?`, w.kind).Scan(&r.kind, &r.ended, &r.held, &r.outcome); err != nil {
					t.Fatal(err)
				}
				if r != w {
					t.Errorf("round %+v, want %+v", r, w)
				}
			}
		})
	}
}
