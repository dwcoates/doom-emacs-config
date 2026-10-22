package wsm

import (
	"context"
	"reflect"
	"testing"

	"agent-repld/internal/dlog"
)

func untoldOf(t *testing.T, s *store, subscriber WorkspaceID, repo RepoKey) (UntoldMergeNotice, bool) {
	t.Helper()
	n, found, err := s.UntoldMergeNotice(context.Background(), subscriber, repo)
	if err != nil {
		t.Fatalf("UntoldMergeNotice: %v", err)
	}
	return n, found
}

func TestNoUntoldNoticeIsRecordedForAFreshSubscriber(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)

	// Act
	_, found := untoldOf(t, s, ws.ID, RepoKey("/repos/doom/.git"))

	// Assert
	if found {
		t.Fatal("a fresh subscriber has an untold notice recorded")
	}
}

func TestAnUntoldNoticeRoundTrips(t *testing.T) {
	tests := []struct {
		name string
		turn TurnID
	}{
		{name: "a submitted notice", turn: "turn-1"},
		{name: "a notice whose submission failed", turn: ""},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			ws := testWorkspace(t, s)
			want := UntoldMergeNotice{Subscriber: ws.ID, Repo: "/repos/doom/.git", Turn: tt.turn, Sentences: []string{"one moved.", "two landed."}}

			// Act
			if err := s.RecordUntoldMergeNotice(context.Background(), want); err != nil {
				t.Fatalf("RecordUntoldMergeNotice: %v", err)
			}
			got, found := untoldOf(t, s, ws.ID, want.Repo)

			// Assert
			if !found || !reflect.DeepEqual(got, want) {
				t.Fatalf("untold = %+v (%v), want %+v", got, found, want)
			}
		})
	}
}

func TestRecordingAnUntoldNoticeReplacesTheOneBefore(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	repo := RepoKey("/repos/doom/.git")
	if err := s.RecordUntoldMergeNotice(context.Background(), UntoldMergeNotice{Subscriber: ws.ID, Repo: repo, Turn: "turn-1", Sentences: []string{"one."}}); err != nil {
		t.Fatal(err)
	}
	want := UntoldMergeNotice{Subscriber: ws.ID, Repo: repo, Turn: "turn-2", Sentences: []string{"one.", "two."}}

	// Act
	if err := s.RecordUntoldMergeNotice(context.Background(), want); err != nil {
		t.Fatal(err)
	}

	// Assert
	if got, _ := untoldOf(t, s, ws.ID, repo); !reflect.DeepEqual(got, want) {
		t.Fatalf("untold = %+v, want the newer notice", got)
	}
}

func TestEachQueuesUntoldNoticeIsItsOwn(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	if err := s.RecordUntoldMergeNotice(context.Background(), UntoldMergeNotice{Subscriber: ws.ID, Repo: "/repos/doom/.git", Turn: "turn-1", Sentences: []string{"one."}}); err != nil {
		t.Fatal(err)
	}

	// Act
	_, found := untoldOf(t, s, ws.ID, "/repos/other/.git")

	// Assert
	if found {
		t.Fatal("another queue's untold notice was answered")
	}
}

func TestAnUntoldNoticeOfNoChangeIsRefusedAndRecorded(t *testing.T) {
	// Arrange
	s, log := testStore(t)
	ws := testWorkspace(t, s)

	// Act
	err := s.RecordUntoldMergeNotice(context.Background(), UntoldMergeNotice{Subscriber: ws.ID, Repo: "/repos/doom/.git", Turn: "turn-1"})

	// Assert
	if err == nil {
		t.Fatal("an untold notice of no change was recorded")
	}
	if _, found := untoldOf(t, s, ws.ID, "/repos/doom/.git"); found {
		t.Fatal("the refused notice was written")
	}
	if _, ok := dlog.FindRecord(log.Records(), dlog.RecordQuery{Level: "error", Operation: "daemon.wsm.record_untold_merge_notice", Message: "refused an untold merge notice that says no change"}); !ok {
		t.Fatalf("no error record of the refusal: %+v", log.Records())
	}
}

func TestClosingAWorkspaceEndsItsUntoldNotices(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	closing := testWorkspace(t, s)
	other := testWorkspaceNamed(t, s, "other")
	for _, ws := range []WorkspaceID{closing.ID, other.ID} {
		if err := s.RecordUntoldMergeNotice(context.Background(), UntoldMergeNotice{Subscriber: ws, Repo: "/repos/doom/.git", Turn: "turn-1", Sentences: []string{"one."}}); err != nil {
			t.Fatal(err)
		}
	}

	// Act
	if err := s.MarkClosed(context.Background(), closing.ID, Closure{At: instant, By: CloseByUser}); err != nil {
		t.Fatalf("MarkClosed: %v", err)
	}

	// Assert
	_, closingFound := untoldOf(t, s, closing.ID, "/repos/doom/.git")
	_, otherFound := untoldOf(t, s, other.ID, "/repos/doom/.git")
	if closingFound || !otherFound {
		t.Fatalf("untold after the close: closing %v, other %v; want only the other's kept", closingFound, otherFound)
	}
}
