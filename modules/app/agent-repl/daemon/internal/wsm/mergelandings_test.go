package wsm

import (
	"context"
	"errors"
	"reflect"
	"testing"
	"time"
)

// testProcessStarted is when the test binary's package variables were set,
// before any test made a store.
var testProcessStarted = time.Now()

// theLandingRepo is the repository every landing test lands on.
const theLandingRepo = RepoKey("/repos/a/.git")

// mergeLanding is a landing of HEAD by PATH at second AT.
func mergeLanding(head string, path LandingPath, at int64) MergeLanding {
	return MergeLanding{
		Repo: theLandingRepo, Head: head, Path: path, Branch: "branch-" + head,
		Workspace: WorkspaceID("ws-" + head), WorkspaceName: "name-" + head, LandedAt: time.Unix(at, 0).UTC(),
	}
}

// recordMergeLandings records each landing, failing the test on an error.
func recordMergeLandings(t *testing.T, s DB, landings ...MergeLanding) {
	t.Helper()
	for _, landing := range landings {
		if _, err := s.RecordMergeLanding(context.Background(), landing); err != nil {
			t.Fatalf("RecordMergeLanding(%s): %v", landing.Head, err)
		}
	}
}

func TestAMergeLandingIsReadBackWhole(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	want := mergeLanding("h1", LandingQueueBypass, 100)

	// Act
	added, err := s.RecordMergeLanding(context.Background(), want)

	// Assert
	got, _, readErr := s.MergeLandingsIn(context.Background(), theLandingRepo, time.Unix(0, 0), time.Unix(1000, 0))
	if err != nil || readErr != nil || !added || !reflect.DeepEqual(got, []MergeLanding{want}) {
		t.Fatalf("recorded (added %v, %v), read %+v, %v; want exactly %+v", added, err, got, readErr, want)
	}
}

func TestRecordingTheSameHeadTwiceKeepsTheFirstPath(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	first := mergeLanding("h1", LandingDeployMediator, 100)
	second := first
	second.Path, second.LandedAt = LandingDirect, time.Unix(101, 0).UTC()
	recordMergeLandings(t, s, first)

	// Act
	added, err := s.RecordMergeLanding(context.Background(), second)

	// Assert
	got, _, readErr := s.MergeLandingsIn(context.Background(), theLandingRepo, time.Unix(0, 0), time.Unix(1000, 0))
	if err != nil || readErr != nil || added || !reflect.DeepEqual(got, []MergeLanding{first}) {
		t.Fatalf("second record (added %v, %v), read %+v, %v; want only the first", added, err, got, readErr)
	}
}

func TestMergeLandingsInHoldsItsWindowEdgesAndItsRepositoryOnly(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	before := mergeLanding("before", LandingThroughQueue, 99)
	atSince := mergeLanding("at-since", LandingThroughQueue, 100)
	inside := mergeLanding("inside", LandingQueueBypass, 150)
	atUntil := mergeLanding("at-until", LandingDeployMediator, 200)
	after := mergeLanding("after", LandingThroughQueue, 201)
	elsewhere := mergeLanding("elsewhere", LandingThroughQueue, 150)
	elsewhere.Repo = "/repos/b/.git"
	recordMergeLandings(t, s, after, inside, atUntil, before, atSince, elsewhere)

	// Act
	got, _, err := s.MergeLandingsIn(context.Background(), theLandingRepo, time.Unix(100, 0), time.Unix(200, 0))

	// Assert
	want := []MergeLanding{atSince, inside, atUntil}
	if err != nil || !reflect.DeepEqual(got, want) {
		t.Fatalf("MergeLandingsIn = %+v, %v; want both edges and the inside, oldest first: %+v", got, err, want)
	}
}

func TestRecordingOfLandingsBeganWhenTheTableWasMade(t *testing.T) {
	// Arrange: a test store is a copy of one template the test process
	// created, so its tables were made after the process started and before
	// this read.
	s, _ := testStore(t)

	// Act
	_, began, err := s.MergeLandingsIn(context.Background(), theLandingRepo, time.Unix(0, 0), time.Now())

	// Assert
	if err != nil || began.Before(testProcessStarted.Add(-time.Second)) || began.After(time.Now().Add(time.Second)) {
		t.Fatalf("recording began %v (%v); want when the test process made the store's tables, after %v", began, err, testProcessStarted)
	}
}

func TestAnIncompleteMergeLandingIsRefused(t *testing.T) {
	tests := []struct {
		name   string
		mutate func(*MergeLanding)
	}{
		{name: "no repository", mutate: func(l *MergeLanding) { l.Repo = "" }},
		{name: "no head", mutate: func(l *MergeLanding) { l.Head = "" }},
		{name: "no branch", mutate: func(l *MergeLanding) { l.Branch = "" }},
		{name: "no instant", mutate: func(l *MergeLanding) { l.LandedAt = time.Time{} }},
		{name: "an unknown path", mutate: func(l *MergeLanding) { l.Path = "sideways" }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, log := testStore(t)
			landing := mergeLanding("h1", LandingThroughQueue, 100)
			tt.mutate(&landing)

			// Act
			added, err := s.RecordMergeLanding(context.Background(), landing)

			// Assert
			if !errors.Is(err, ErrIncompleteLanding) || added {
				t.Fatalf("RecordMergeLanding = %v, %v; want ErrIncompleteLanding", added, err)
			}
			if n := scalar[int](t, s, `SELECT COUNT(*) FROM merge_landings`); n != 0 {
				t.Fatalf("%d landings recorded after a refusal, want none", n)
			}
			found := false
			for _, r := range log.Records() {
				found = found || (r.Level == "error" && r.Operation == "daemon.wsm.record_merge_landing" && r.Context["head"] == landing.Head)
			}
			if !found {
				t.Fatalf("no daemon.wsm.record_merge_landing ERROR naming the landing in %+v", log.Records())
			}
		})
	}
}

func TestAReversedLandingWindowIsRefused(t *testing.T) {
	// Arrange
	s, log := testStore(t)

	// Act
	_, _, err := s.MergeLandingsIn(context.Background(), theLandingRepo, time.Unix(200, 0), time.Unix(100, 0))

	// Assert
	if err == nil {
		t.Fatal("MergeLandingsIn accepted a window ending before it starts")
	}
	found := false
	for _, r := range log.Records() {
		found = found || (r.Level == "error" && r.Operation == "daemon.wsm.merge_landings_in")
	}
	if !found {
		t.Fatalf("no daemon.wsm.merge_landings_in ERROR in %+v", log.Records())
	}
}
