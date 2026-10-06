package wsm

import (
	"bytes"
	"context"
	"errors"
	"strings"
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
		{name: "kept items with no digest", run: NewsDigestRun{EndedAt: at, Recorded: true, History: history(at, keptItem("i", ""))}},
		{name: "kept items covering no span", run: NewsDigestRun{EndedAt: at, Recorded: true, Digest: minted("d"),
			History: &NewsDigestHistory{KeepSince: at, Items: []NewsDigestKeptItem{keptItem("i", "")}}}},
		{name: "kept items covering past the run's end", run: NewsDigestRun{EndedAt: at, Recorded: true, Digest: minted("d"),
			History: &NewsDigestHistory{CoversFrom: at.Add(time.Hour), KeepSince: at, Items: []NewsDigestKeptItem{keptItem("i", "")}}}},
		{name: "kept items pruning nothing", run: NewsDigestRun{EndedAt: at, Recorded: true, Digest: minted("d"),
			History: &NewsDigestHistory{CoversFrom: at, Items: []NewsDigestKeptItem{keptItem("i", "")}}}},
		{name: "kept items pruning past the run's end", run: NewsDigestRun{EndedAt: at, Recorded: true, Digest: minted("d"),
			History: &NewsDigestHistory{CoversFrom: at, KeepSince: at.Add(time.Hour), Items: []NewsDigestKeptItem{keptItem("i", "")}}}},
		{name: "a history with no items", run: NewsDigestRun{EndedAt: at, Recorded: true, Digest: minted("d"), History: history(at)}},
		{name: "a kept item with no encoding", run: NewsDigestRun{EndedAt: at, Recorded: true, Digest: minted("d"), History: history(at, keptItem("", "r"))}},
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
	s.current.Store(&handleState{db: s.db(), readOnly: true})

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
	handle, err := Open(context.Background(), path, WithUnsyncedWrites())
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

func TestAMintedDigestKeepsItsOverlayAndMintInstantAfterADismiss(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	recordRun(t, s, NewsDigestRun{EndedAt: at, Recorded: true, Digest: &NewsDigestMinted{ID: "d1", Overlay: []byte("o")}})

	// Act
	if _, err := s.DismissNewsDigest(context.Background(), "d1"); err != nil {
		t.Fatalf("DismissNewsDigest: %v", err)
	}

	// Assert
	state := loadState(t, s)
	if !bytes.Equal(state.LatestOverlay, []byte("o")) || !state.LatestMadeAt.Equal(at) {
		t.Fatalf("state = %+v, want the overlay and its mint instant kept", state)
	}
}

func TestRestandingADismissedDigestStandsItAgain(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	recordRun(t, s, NewsDigestRun{EndedAt: at, Recorded: true, Digest: &NewsDigestMinted{ID: "d1", Overlay: []byte("o")}})
	if _, err := s.DismissNewsDigest(context.Background(), "d1"); err != nil {
		t.Fatalf("DismissNewsDigest: %v", err)
	}

	// Act
	ok, err := s.RestandNewsDigest(context.Background(), "d1")

	// Assert
	if err != nil || !ok {
		t.Fatalf("RestandNewsDigest = (%v, %v), want (true, nil)", ok, err)
	}
	if state := loadState(t, s); !bytes.Equal(state.Standing, []byte("o")) {
		t.Fatalf("standing = %q, want the digest standing again", state.Standing)
	}
}

func TestRestandingAStandingDigestIsNoMatch(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	recordRun(t, s, NewsDigestRun{EndedAt: at, Recorded: true, Digest: &NewsDigestMinted{ID: "d1", Overlay: []byte("o")}})

	// Act
	ok, err := s.RestandNewsDigest(context.Background(), "d1")

	// Assert
	if err != nil || ok {
		t.Fatalf("RestandNewsDigest = (%v, %v), want (false, nil)", ok, err)
	}
}

func TestRestandingAnOlderDigestChangesNothing(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	recordRun(t, s, NewsDigestRun{EndedAt: at, Recorded: true, Digest: &NewsDigestMinted{ID: "d1", Overlay: []byte("o1")}})
	recordRun(t, s, NewsDigestRun{EndedAt: at.Add(time.Hour), Recorded: true, Digest: &NewsDigestMinted{ID: "d2", Overlay: []byte("o2")}})
	if _, err := s.DismissNewsDigest(context.Background(), "d2"); err != nil {
		t.Fatalf("DismissNewsDigest: %v", err)
	}

	// Act
	ok, err := s.RestandNewsDigest(context.Background(), "d1")

	// Assert
	if err != nil || ok {
		t.Fatalf("RestandNewsDigest(d1) = (%v, %v), want (false, nil)", ok, err)
	}
	if state := loadState(t, s); state.Standing != nil {
		t.Fatalf("standing = %q, want nothing standing", state.Standing)
	}
}

func TestRestandRefusesAnEmptyID(t *testing.T) {
	// Arrange
	s, log := testStore(t)

	// Act
	_, err := s.RestandNewsDigest(context.Background(), "")

	// Assert
	if err == nil {
		t.Fatal("RestandNewsDigest(\"\") = nil, want a refusal")
	}
	if !loggedOperation(log, "daemon.wsm.restand_news_digest", "error") {
		t.Fatalf("the refusal was not recorded at ERROR: %v", log.Records())
	}
}

func TestNoteEditorInstance(t *testing.T) {
	tests := []struct {
		name    string
		earlier []string
		note    string
		want    bool
	}{
		{name: "the first instance ever is new", note: "e1", want: true},
		{name: "the same instance again is a reconnect", earlier: []string{"e1"}, note: "e1", want: false},
		{name: "a different instance is a restart", earlier: []string{"e1"}, note: "e2", want: true},
		{name: "an instance seen before the last one is new again", earlier: []string{"e1", "e2"}, note: "e1", want: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			for _, e := range tt.earlier {
				if _, err := s.NoteEditorInstance(context.Background(), e, at); err != nil {
					t.Fatalf("NoteEditorInstance(%q): %v", e, err)
				}
			}

			// Act
			got, err := s.NoteEditorInstance(context.Background(), tt.note, at)

			// Assert
			if err != nil || got != tt.want {
				t.Fatalf("NoteEditorInstance(%q) = (%v, %v), want (%v, nil)", tt.note, got, err, tt.want)
			}
		})
	}
}

func TestNoteEditorInstanceRefusesAnEmptyInstance(t *testing.T) {
	// Arrange
	s, log := testStore(t)

	// Act
	_, err := s.NoteEditorInstance(context.Background(), "", at)

	// Assert
	if err == nil {
		t.Fatal("NoteEditorInstance(\"\") = nil, want a refusal")
	}
	if !loggedOperation(log, "daemon.wsm.note_editor_instance", "error") {
		t.Fatalf("the refusal was not recorded at ERROR: %v", log.Records())
	}
}

func TestTheMigrationAddsTheRedisplayColumnsAndTheEditorInstanceTable(t *testing.T) {
	// Arrange
	path := fixtureAt(t, 19)

	// Act
	handle, err := Open(context.Background(), path, WithUnsyncedWrites())
	if err != nil {
		t.Fatalf("Open on a layout-19 database: %v", err)
	}
	defer handle.Close()

	// Assert
	s := handle.(*store)
	if got := scalar[int](t, s, `SELECT count(*) FROM pragma_table_info('news_digest') WHERE name IN ('latest_overlay', 'latest_made_at')`); got != 2 {
		t.Fatalf("redisplay columns after the migration = %d, want 2", got)
	}
	if got := scalar[int](t, s, `SELECT count(*) FROM sqlite_master WHERE type = 'table' AND name = 'editor_instance'`); got != 1 {
		t.Fatalf("editor_instance tables after the migration = %d, want 1", got)
	}
}

// minted is a digest named id with a placeholder overlay.
func minted(id string) *NewsDigestMinted {
	return &NewsDigestMinted{ID: id, Overlay: []byte("overlay-" + id)}
}

// keptItem is a kept item with encoding item and risk reason risk.
func keptItem(item, risk string) NewsDigestKeptItem {
	return NewsDigestKeptItem{Item: []byte(item), Risk: risk}
}

// history keeps items for a run ending at ended: it covers from ended and
// prunes nothing newer than fourteen days before it.
func history(ended time.Time, items ...NewsDigestKeptItem) *NewsDigestHistory {
	return &NewsDigestHistory{CoversFrom: ended, KeepSince: ended.Add(-14 * 24 * time.Hour), Items: items}
}

// risksSince loads the marked items since since, failing the test on an error.
func risksSince(t *testing.T, s *store, since time.Time) []NewsDigestRisk {
	t.Helper()
	risks, err := s.NewsDigestRisksSince(context.Background(), since)
	if err != nil {
		t.Fatalf("NewsDigestRisksSince: %v", err)
	}
	return risks
}

func TestNewsDigestRisksSinceAnswersOnlyMarkedItems(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	recordRun(t, s, NewsDigestRun{EndedAt: at, Recorded: true, Digest: minted("d1"),
		History: history(at, keptItem("plain", ""), keptItem("risky", "breaks the shim"))})

	// Act
	risks := risksSince(t, s, at.Add(-time.Hour))

	// Assert
	if len(risks) != 1 || string(risks[0].Item) != "risky" || risks[0].Reason != "breaks the shim" || !risks[0].RunEnd.Equal(at) {
		t.Fatalf("risks = %+v, want only the marked item", risks)
	}
}

func TestNewsDigestRisksSinceOrdersByRunThenPosition(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	later := at.Add(24 * time.Hour)
	recordRun(t, s, NewsDigestRun{EndedAt: later, Recorded: true, Digest: minted("d2"),
		History: history(later, keptItem("c", "r"), keptItem("d", "r"))})
	recordRun(t, s, NewsDigestRun{EndedAt: at, Recorded: true, Digest: minted("d1"),
		History: history(at, keptItem("a", "r"), keptItem("b", "r"))})

	// Act
	risks := risksSince(t, s, at.Add(-time.Hour))

	// Assert
	var got []string
	for _, r := range risks {
		got = append(got, string(r.Item))
	}
	if strings.Join(got, ",") != "a,b,c,d" {
		t.Fatalf("risk order = %v, want a,b,c,d", got)
	}
}

func TestNewsDigestRisksSinceWindow(t *testing.T) {
	tests := []struct {
		name  string
		since time.Time
		want  int
	}{
		{name: "a run ending after since is in", since: at.Add(-time.Minute), want: 1},
		{name: "a run ending exactly at since is in", since: at, want: 1},
		{name: "a run ending before since is out", since: at.Add(time.Minute), want: 0},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			recordRun(t, s, NewsDigestRun{EndedAt: at, Recorded: true, Digest: minted("d1"), History: history(at, keptItem("i", "r"))})

			// Act
			risks := risksSince(t, s, tt.since)

			// Assert
			if len(risks) != tt.want {
				t.Fatalf("risks = %d, want %d", len(risks), tt.want)
			}
		})
	}
}

func TestKeepingItemsPrunesRunsEndedBeforeKeepSince(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	old := at.Add(-15 * 24 * time.Hour)
	recent := at.Add(-13 * 24 * time.Hour)
	recordRun(t, s, NewsDigestRun{EndedAt: old, Recorded: true, Digest: minted("d0"), History: history(old, keptItem("old", "r"))})
	recordRun(t, s, NewsDigestRun{EndedAt: recent, Recorded: true, Digest: minted("d1"), History: history(recent, keptItem("recent", "r"))})

	// Act
	recordRun(t, s, NewsDigestRun{EndedAt: at, Recorded: true, Digest: minted("d2"), History: history(at, keptItem("now", ""))})

	// Assert
	if got := scalar[int](t, s, `SELECT count(*) FROM news_digest_items`); got != 2 {
		t.Fatalf("kept items after the prune = %d, want 2 (the fifteen-day-old run pruned)", got)
	}
	if risks := risksSince(t, s, old.Add(-time.Hour)); len(risks) != 1 || string(risks[0].Item) != "recent" {
		t.Fatalf("risks = %+v, want only the recent one", risks)
	}
}

func TestARunWithNoHistoryPrunesNothing(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	old := at.Add(-30 * 24 * time.Hour)
	recordRun(t, s, NewsDigestRun{EndedAt: old, Recorded: true, Digest: minted("d0"), History: history(old, keptItem("old", "r"))})

	// Act
	recordRun(t, s, NewsDigestRun{EndedAt: at, Recorded: true})

	// Assert
	if got := scalar[int](t, s, `SELECT count(*) FROM news_digest_items`); got != 1 {
		t.Fatalf("kept items = %d, want 1", got)
	}
}

func TestTheFirstKeptRunStartsTheHistorysSpan(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	first := at.Add(-3 * time.Hour)
	recordRun(t, s, NewsDigestRun{EndedAt: at, Recorded: true, Digest: minted("d1"),
		History: &NewsDigestHistory{CoversFrom: first, KeepSince: first, Items: []NewsDigestKeptItem{keptItem("a", "")}}})
	later := at.Add(24 * time.Hour)

	// Act
	recordRun(t, s, NewsDigestRun{EndedAt: later, Recorded: true, Digest: minted("d2"), History: history(later, keptItem("b", ""))})

	// Assert
	if state := loadState(t, s); !state.HistorySince.Equal(first) {
		t.Fatalf("history since = %v, want the first kept run's span start %v", state.HistorySince, first)
	}
}

func TestHistorySinceIsZeroBeforeAnyItemIsKept(t *testing.T) {
	// Arrange
	s, _ := testStore(t)

	// Act
	recordRun(t, s, NewsDigestRun{EndedAt: at, Recorded: true, Digest: minted("d1")})

	// Assert
	if state := loadState(t, s); !state.HistorySince.IsZero() {
		t.Fatalf("history since = %v, want zero", state.HistorySince)
	}
}

func TestAMarkedRowWithABlankReasonIsADecodeError(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	if _, err := s.db().Exec(`INSERT INTO news_digest_items (run_end, position, item, risk) VALUES (?, 0, x'01', '')`, nanos(at)); err != nil {
		t.Fatalf("seed: %v", err)
	}

	// Act
	_, err := s.NewsDigestRisksSince(context.Background(), at.Add(-time.Hour))

	// Assert
	var decode *DecodeError
	if !errors.As(err, &decode) {
		t.Fatalf("NewsDigestRisksSince = %v, want a DecodeError", err)
	}
}

func TestTheMigrationAddsTheNewsDigestHistory(t *testing.T) {
	// Arrange
	path := fixtureAt(t, 21)

	// Act
	handle, err := Open(context.Background(), path, WithUnsyncedWrites())
	if err != nil {
		t.Fatalf("Open on a layout-21 database: %v", err)
	}
	defer handle.Close()

	// Assert
	s := handle.(*store)
	if got := scalar[int](t, s, `SELECT count(*) FROM sqlite_master WHERE type = 'table' AND name = 'news_digest_items'`); got != 1 {
		t.Fatalf("news_digest_items tables after the migration = %d, want 1", got)
	}
	if got := scalar[int](t, s, `SELECT count(*) FROM pragma_table_info('news_digest') WHERE name = 'history_since'`); got != 1 {
		t.Fatalf("history_since columns after the migration = %d, want 1", got)
	}
}
