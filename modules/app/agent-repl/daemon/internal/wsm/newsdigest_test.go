package wsm

import (
	"bytes"
	"context"
	"errors"
	"testing"
	"time"
)

// at is a fixed instant the news digest tests record against.
var at = time.Date(2026, 10, 2, 9, 0, 0, 0, time.UTC)

// recordRun records run on s, failing the test on an error.
func recordRun(t *testing.T, s *store, run NewsDigestRun) {
	t.Helper()
	if err := s.RecordNewsDigestRun(context.Background(), run); err != nil {
		t.Fatalf("RecordNewsDigestRun: %v", err)
	}
}

// loadState loads the news digest state from s, failing the test on an error.
func loadState(t *testing.T, s *store) NewsDigestState {
	t.Helper()
	state, err := s.NewsDigestState(context.Background())
	if err != nil {
		t.Fatalf("NewsDigestState: %v", err)
	}
	return state
}

func TestNewsDigestStateIsZeroBeforeAnyRun(t *testing.T) {
	// Arrange
	s, _ := testStore(t)

	// Act
	state := loadState(t, s)

	// Assert
	if !state.LastRunEnd.IsZero() || !state.Baseline.IsZero() || state.LatestID != "" || state.Standing != nil || len(state.Snapshots) != 0 {
		t.Fatalf("state = %+v, want the zero state", state)
	}
}

func TestARecordedRunSetsTheCadenceOriginAndTheBaseline(t *testing.T) {
	// Arrange
	s, _ := testStore(t)

	// Act
	recordRun(t, s, NewsDigestRun{EndedAt: at, Recorded: true})

	// Assert
	state := loadState(t, s)
	if !state.LastRunEnd.Equal(at) || !state.Baseline.Equal(at) {
		t.Fatalf("last run end = %v, baseline = %v, want both %v", state.LastRunEnd, state.Baseline, at)
	}
}

func TestAnUnrecordedRunMovesOnlyTheCadenceOrigin(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	recordRun(t, s, NewsDigestRun{EndedAt: at, Recorded: true})
	later := at.Add(24 * time.Hour)

	// Act
	recordRun(t, s, NewsDigestRun{EndedAt: later})

	// Assert
	state := loadState(t, s)
	if !state.LastRunEnd.Equal(later) || !state.Baseline.Equal(at) {
		t.Fatalf("last run end = %v, baseline = %v, want %v and %v", state.LastRunEnd, state.Baseline, later, at)
	}
}

func TestARecordedRunReplacesTheSnapshotsItNames(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	recordRun(t, s, NewsDigestRun{EndedAt: at, Recorded: true, Snapshots: map[string]string{"a": "old", "b": "kept"}})

	// Act
	recordRun(t, s, NewsDigestRun{EndedAt: at.Add(time.Hour), Recorded: true, Snapshots: map[string]string{"a": "new"}})

	// Assert
	state := loadState(t, s)
	if state.Snapshots["a"] != "new" || state.Snapshots["b"] != "kept" || len(state.Snapshots) != 2 {
		t.Fatalf("snapshots = %v, want a=new and b=kept", state.Snapshots)
	}
}

func TestARunThatMakesADigestLeavesItStanding(t *testing.T) {
	// Arrange
	s, _ := testStore(t)

	// Act
	recordRun(t, s, NewsDigestRun{EndedAt: at, Recorded: true, Digest: &NewsDigestMinted{ID: "d1", Overlay: []byte("overlay")}})

	// Assert
	state := loadState(t, s)
	if state.LatestID != "d1" || !bytes.Equal(state.Standing, []byte("overlay")) {
		t.Fatalf("latest = %q, standing = %q, want d1 standing", state.LatestID, state.Standing)
	}
}

func TestARunWithNoDigestLeavesTheStandingDigestStanding(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	recordRun(t, s, NewsDigestRun{EndedAt: at, Recorded: true, Digest: &NewsDigestMinted{ID: "d1", Overlay: []byte("overlay")}})

	// Act
	recordRun(t, s, NewsDigestRun{EndedAt: at.Add(time.Hour), Recorded: true})

	// Assert
	if state := loadState(t, s); state.LatestID != "d1" || state.Standing == nil {
		t.Fatalf("latest = %q, standing = %q, want d1 still standing", state.LatestID, state.Standing)
	}
}

func TestRecordNewsDigestRunRefusesMalformedRuns(t *testing.T) {
	tests := []struct {
		name string
		run  NewsDigestRun
	}{
		{name: "no end", run: NewsDigestRun{Recorded: true}},
		{name: "snapshots on an unrecorded run", run: NewsDigestRun{EndedAt: at, Snapshots: map[string]string{"a": "x"}}},
		{name: "a digest on an unrecorded run", run: NewsDigestRun{EndedAt: at, Digest: &NewsDigestMinted{ID: "d", Overlay: []byte("o")}}},
		{name: "a digest with no id", run: NewsDigestRun{EndedAt: at, Recorded: true, Digest: &NewsDigestMinted{Overlay: []byte("o")}}},
		{name: "a digest with no overlay", run: NewsDigestRun{EndedAt: at, Recorded: true, Digest: &NewsDigestMinted{ID: "d"}}},
		{name: "a snapshot naming no source", run: NewsDigestRun{EndedAt: at, Recorded: true, Snapshots: map[string]string{"": "x"}}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, log := testStore(t)

			// Act
			err := s.RecordNewsDigestRun(context.Background(), tt.run)

			// Assert
			if err == nil {
				t.Fatal("RecordNewsDigestRun = nil, want a refusal")
			}
			if !loggedOperation(log, "daemon.wsm.record_news_digest_run", "error") {
				t.Fatalf("the refusal was not recorded at ERROR: %v", log.Records())
			}
			if state := loadState(t, s); !state.LastRunEnd.IsZero() {
				t.Fatalf("a refused run was recorded: %+v", state)
			}
		})
	}
}

func TestRecordNewsDigestRunRefusesAReadOnlyHandle(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	s.readOnly = true

	// Act
	err := s.RecordNewsDigestRun(context.Background(), NewsDigestRun{EndedAt: at, Recorded: true})

	// Assert
	if !errors.Is(err, ErrReadOnly) {
		t.Fatalf("RecordNewsDigestRun = %v, want ErrReadOnly", err)
	}
}

func TestDismissingTheStandingDigestTakesItDown(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	recordRun(t, s, NewsDigestRun{EndedAt: at, Recorded: true, Digest: &NewsDigestMinted{ID: "d1", Overlay: []byte("o")}})

	// Act
	ok, err := s.DismissNewsDigest(context.Background(), "d1")

	// Assert
	if err != nil || !ok {
		t.Fatalf("DismissNewsDigest = (%v, %v), want (true, nil)", ok, err)
	}
	if state := loadState(t, s); state.Standing != nil || state.LatestID != "d1" {
		t.Fatalf("state = %+v, want d1 minted and nothing standing", state)
	}
}

func TestDismissingTheSameDigestTwiceIsStillAMatch(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	recordRun(t, s, NewsDigestRun{EndedAt: at, Recorded: true, Digest: &NewsDigestMinted{ID: "d1", Overlay: []byte("o")}})
	if _, err := s.DismissNewsDigest(context.Background(), "d1"); err != nil {
		t.Fatalf("first dismiss: %v", err)
	}

	// Act
	ok, err := s.DismissNewsDigest(context.Background(), "d1")

	// Assert
	if err != nil || !ok {
		t.Fatalf("second DismissNewsDigest = (%v, %v), want (true, nil)", ok, err)
	}
}

func TestDismissingAnOlderDigestChangesNothing(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	recordRun(t, s, NewsDigestRun{EndedAt: at, Recorded: true, Digest: &NewsDigestMinted{ID: "d1", Overlay: []byte("o1")}})
	recordRun(t, s, NewsDigestRun{EndedAt: at.Add(time.Hour), Recorded: true, Digest: &NewsDigestMinted{ID: "d2", Overlay: []byte("o2")}})

	// Act
	ok, err := s.DismissNewsDigest(context.Background(), "d1")

	// Assert
	if err != nil || ok {
		t.Fatalf("DismissNewsDigest(d1) = (%v, %v), want (false, nil)", ok, err)
	}
	if state := loadState(t, s); !bytes.Equal(state.Standing, []byte("o2")) {
		t.Fatalf("standing = %q, want d2 still standing", state.Standing)
	}
}

func TestDismissingBeforeAnyDigestIsNoMatch(t *testing.T) {
	// Arrange
	s, _ := testStore(t)

	// Act
	ok, err := s.DismissNewsDigest(context.Background(), "d1")

	// Assert
	if err != nil || ok {
		t.Fatalf("DismissNewsDigest = (%v, %v), want (false, nil)", ok, err)
	}
}

func TestDismissRefusesAnEmptyID(t *testing.T) {
	// Arrange
	s, log := testStore(t)

	// Act
	_, err := s.DismissNewsDigest(context.Background(), "")

	// Assert
	if err == nil {
		t.Fatal("DismissNewsDigest(\"\") = nil, want a refusal")
	}
	if !loggedOperation(log, "daemon.wsm.dismiss_news_digest", "error") {
		t.Fatalf("the refusal was not recorded at ERROR: %v", log.Records())
	}
}

func TestAStandingDigestWithNoIDIsADecodeError(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	if _, err := s.db().Exec(`INSERT INTO news_digest (id, last_run_end, standing) VALUES (1, 1, x'00')`); err != nil {
		t.Fatalf("seed: %v", err)
	}

	// Act
	_, err := s.NewsDigestState(context.Background())

	// Assert
	var decode *DecodeError
	if !errors.As(err, &decode) {
		t.Fatalf("NewsDigestState = %v, want a DecodeError", err)
	}
}

func TestTheMigrationAddsTheNewsDigestTables(t *testing.T) {
	// Arrange
	path := layout3Fixture(t)

	// Act
	handle, err := Open(context.Background(), path)
	if err != nil {
		t.Fatalf("Open on a layout-3 database: %v", err)
	}
	defer handle.Close()

	// Assert
	s := handle.(*store)
	got := scalar[int](t, s, `SELECT count(*) FROM sqlite_master WHERE type = 'table' AND name IN ('news_digest', 'news_digest_sources')`)
	if got != 2 {
		t.Fatalf("news digest tables after the migration = %d, want 2", got)
	}
}
