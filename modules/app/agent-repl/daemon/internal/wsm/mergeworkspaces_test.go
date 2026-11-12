package wsm

import (
	"context"
	"errors"
	"reflect"
	"strings"
	"testing"
	"time"
)

// registerMergeWorkspace registers a merge workspace forked from requester for
// request, in a fresh temp dir.
func registerMergeWorkspace(t *testing.T, s *store, requester WorkspaceID, request DetachedMergeID) Workspace {
	t.Helper()
	dir := t.TempDir()
	ws, _, err := s.RegisterWorkspace(context.Background(), dir, RegisterFacts{
		Name: "sample-merge", Branch: "sample-merge", ParentBranch: "master", Parent: &requester, RepoDir: dir,
		MergeOf: &MergeOrigin{Requester: requester, Request: request},
	})
	if err != nil {
		t.Fatalf("RegisterWorkspace: %v", err)
	}
	return ws
}

// requestDetached records a detached own-branch request for ws in repo.
func requestDetached(t *testing.T, s *store, repo RepoKey, ws WorkspaceID, request DetachedMergeID) {
	t.Helper()
	asyncRequest(t, s, repo, ws, MergeSource{Kind: MergeSourceBranch, Branch: "feature", Detach: request, DetachMaker: "maker", DetachName: "sample"})
}

// asyncRequest records SOURCE as an async request of WS, asked for in no turn.
func asyncRequest(t *testing.T, s *store, repo RepoKey, ws WorkspaceID, source MergeSource) {
	t.Helper()
	if err := s.RequestAsyncMerge(context.Background(), repo, ws, "", source, time.Unix(100, 0)); err != nil {
		t.Fatalf("RequestAsyncMerge: %v", err)
	}
}

// asyncRequestsOf answers WS's async requests, failing the test on a read error.
func asyncRequestsOf(t *testing.T, s *store, ws WorkspaceID) []AsyncMergeRequest {
	t.Helper()
	requests, err := s.AsyncMergeRequestsOf(context.Background(), ws)
	if err != nil {
		t.Fatalf("AsyncMergeRequestsOf: %v", err)
	}
	return requests
}

func TestRegisterWorkspaceRecordsWhatAMergeWorkspaceWasForkedFor(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	requester := testWorkspace(t, s)
	request := NewDetachedMergeID()

	// Act
	ws := registerMergeWorkspace(t, s, requester.ID, request)
	loaded, err := s.Workspace(context.Background(), ws.ID)

	// Assert
	if err != nil {
		t.Fatalf("Workspace: %v", err)
	}
	if loaded.MergeOf == nil || !reflect.DeepEqual(*loaded.MergeOf, MergeOrigin{Requester: requester.ID, Request: request}) {
		t.Fatalf("MergeOf = %+v, want the requester and the request", loaded.MergeOf)
	}
}

func TestAnOrdinaryWorkspaceIsNoMergeWorkspace(t *testing.T) {
	// Arrange
	s, _ := testStore(t)

	// Act
	ws := testWorkspace(t, s)
	loaded, err := s.Workspace(context.Background(), ws.ID)

	// Assert
	if err != nil {
		t.Fatalf("Workspace: %v", err)
	}
	if loaded.MergeOf != nil {
		t.Fatalf("MergeOf = %+v, want nil", loaded.MergeOf)
	}
}

func TestAWorkspaceRowWithHalfAMergeOriginIsRefused(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s)
	corrupt(t, s, `UPDATE workspaces SET merge_of = 'someone' WHERE id = ?`, ws.ID)

	// Act
	_, err := s.Workspace(context.Background(), ws.ID)

	// Assert
	var decode *DecodeError
	if !errors.As(err, &decode) || decode.Field != "merge_of" {
		t.Fatalf("Workspace = %v, want a merge_of DecodeError", err)
	}
}

func TestMergeWorkspaceForFindsTheWorkspaceForkedForTheRequest(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	requester := testWorkspace(t, s)
	request := NewDetachedMergeID()
	forked := registerMergeWorkspace(t, s, requester.ID, request)

	// Act
	got, found, err := s.MergeWorkspaceFor(context.Background(), request)

	// Assert
	if err != nil || !found || got.ID != forked.ID {
		t.Fatalf("MergeWorkspaceFor = %v, %v, %v; want %s", got.ID, found, err, forked.ID)
	}
}

func TestMergeWorkspaceForAnswersNoneForAnotherRequest(t *testing.T) {
	// Arrange: an earlier request's merge workspace stands.
	s, _ := testStore(t)
	requester := testWorkspace(t, s)
	registerMergeWorkspace(t, s, requester.ID, NewDetachedMergeID())

	// Act
	_, found, err := s.MergeWorkspaceFor(context.Background(), NewDetachedMergeID())

	// Assert
	if err != nil || found {
		t.Fatalf("MergeWorkspaceFor = %v, %v; want none", found, err)
	}
}

func TestRequestAsyncMergeRoundTripsADetachedRequest(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	requester := testWorkspace(t, s)
	repo := RepoKey(t.TempDir())
	request := NewDetachedMergeID()

	// Act
	requestDetached(t, s, repo, requester.ID, request)
	queue, err := s.MergeQueue(context.Background(), repo)

	// Assert: the request is on its own, and the requester holds no queue row.
	key, keyErr := normalizeRepoKey(repo)
	if keyErr != nil {
		t.Fatalf("normalizeRepoKey: %v", keyErr)
	}
	if requests := asyncRequestsOf(t, s, requester.ID); len(requests) != 1 || requests[0].Source.Detach != request || requests[0].Repo != key {
		t.Fatalf("AsyncMergeRequestsOf = %+v; want the detached request", requests)
	}
	if err != nil || len(queue) != 0 {
		t.Fatalf("MergeQueue = %+v, %v; want no row", queue, err)
	}
}

func TestADetachedSourceMergesANamedBranchAndNeverKeepsTheRequesterOpen(t *testing.T) {
	tests := []struct {
		name   string
		source MergeSource
	}{
		{name: "keep open", source: MergeSource{Kind: MergeSourceOwnBranch, KeepOpen: true, Detach: "d", DetachMaker: "maker", DetachName: "n"}},
		{name: "another workspace", source: MergeSource{Kind: MergeSourceWorkspace, Workspace: "w", Detach: "d", DetachMaker: "maker", DetachName: "n"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange in the table. Act.
			err := tt.source.validate()
			// Assert
			if err == nil || !strings.Contains(err.Error(), "detached") {
				t.Fatalf("validate = %v, want a detached-merge refusal", err)
			}
		})
	}
}

func TestADetachedSourceOfANamedBranchIsValid(t *testing.T) {
	tests := []struct {
		name   string
		source MergeSource
	}{
		{name: "a named branch", source: MergeSource{Kind: MergeSourceBranch, Branch: "b", Detach: "d", DetachMaker: "maker", DetachName: "n"}},
		{name: "a named branch with an instruction", source: MergeSource{Kind: MergeSourceBranch, Branch: "b", Detach: "d", DetachMaker: "maker", DetachName: "n", Pick: "rebase it"}},
		{name: "a named branch with landing effects", source: MergeSource{Kind: MergeSourceBranch, Branch: "b", Detach: "d", DetachMaker: "maker", DetachName: "n", CloseOnLanding: "w", CleanupWorktree: true, CleanupDir: "/x"}},
		{name: "a request an earlier layout recorded on the requester's own branch", source: MergeSource{Kind: MergeSourceOwnBranch, Branch: "b", Detach: "d", DetachMaker: "maker", DetachName: "n"}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange in the table. Act.
			err := tt.source.validate()
			// Assert
			if err != nil {
				t.Fatalf("validate = %v, want nil", err)
			}
		})
	}
}

func TestLandingEffectsRideADetachedRequestAlone(t *testing.T) {
	tests := []struct {
		name   string
		source MergeSource
		want   string
	}{
		{name: "closing another workspace in place", source: MergeSource{Kind: MergeSourceOwnBranch, CloseOnLanding: "w"}, want: "async merge's ask"},
		{name: "cleaning up in place", source: MergeSource{Kind: MergeSourceOwnBranch, CleanupWorktree: true}, want: "async merge's ask"},
		{name: "a cleanup directory without cleanup", source: MergeSource{Kind: MergeSourceBranch, Branch: "b", Detach: "d", DetachMaker: "maker", DetachName: "n", CleanupDir: "/x"}, want: "only for a request that cleans up"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange in the table. Act.
			err := tt.source.validate()
			// Assert
			if err == nil || !strings.Contains(err.Error(), tt.want) {
				t.Fatalf("validate = %v, want %q", err, tt.want)
			}
		})
	}
}

func TestADetachedRequestKeepsItsLandingEffects(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	requester := testWorkspace(t, s)
	repo := RepoKey(t.TempDir())
	source := MergeSource{Kind: MergeSourceBranch, Branch: "fix", Detach: "d", DetachMaker: "maker", DetachName: "n", Pick: "rebase it",
		CloseOnLanding: "child", CleanupWorktree: true, CleanupDir: "/wt/fix"}

	// Act
	err := s.RequestAsyncMerge(context.Background(), repo, requester.ID, "", source, time.Now())
	queue, readErr := s.AsyncMergeRequestsOf(context.Background(), requester.ID)

	// Assert
	if err != nil || readErr != nil || len(queue) != 1 {
		t.Fatalf("RequestAsyncMerge = %v, AsyncMergeRequestsOf = %+v, %v; want one request", err, queue, readErr)
	}
	got := queue[0].Source
	if got.CloseOnLanding != "child" || !got.CleanupWorktree || got.CleanupDir != "/wt/fix" || got.Pick != "rebase it" {
		t.Fatalf("Source = %+v, want the landing effects and the instruction kept", got)
	}
}

func TestHandOffDetachedMergeMovesTheRequestOntoTheMergeWorkspace(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	requester := testWorkspace(t, s)
	repo := RepoKey(t.TempDir())
	request := NewDetachedMergeID()
	requestDetached(t, s, repo, requester.ID, request)
	forked := registerMergeWorkspace(t, s, requester.ID, request)

	// Act
	err := s.HandOffDetachedMerge(context.Background(), repo, requester.ID, request, forked.ID, "sample-merge", "", time.Unix(200, 0))

	// Assert: one row, the merge workspace's, an in-place own-branch merge that
	// closes on landing, still requested, and the request gone.
	if err != nil {
		t.Fatalf("HandOffDetachedMerge: %v", err)
	}
	if requests := asyncRequestsOf(t, s, requester.ID); len(requests) != 0 {
		t.Fatalf("AsyncMergeRequestsOf = %+v; want the request handed off", requests)
	}
	queue, err := s.MergeQueue(context.Background(), repo)
	if err != nil || len(queue) != 1 {
		t.Fatalf("MergeQueue = %+v, %v; want one entry", queue, err)
	}
	want := MergeSource{Kind: MergeSourceOwnBranch, Branch: "sample-merge"}
	if queue[0].Workspace != forked.ID || !reflect.DeepEqual(queue[0].Source, want) || queue[0].State != MergeRequested {
		t.Fatalf("entry = %+v, want the merge workspace's requested in-place merge", queue[0])
	}
}

func TestHandOffDetachedMergeCarriesAFrontRequestToTheMergeWorkspace(t *testing.T) {
	// Arrange: a detached merge asked for the front of the queue.
	s, _ := testStore(t)
	requester := testWorkspace(t, s)
	repo := RepoKey(t.TempDir())
	request := NewDetachedMergeID()
	asyncRequest(t, s, repo, requester.ID, MergeSource{Kind: MergeSourceBranch, Branch: "feature", Detach: request, DetachMaker: "maker", DetachName: "sample-merge", Front: true})
	forked := registerMergeWorkspace(t, s, requester.ID, request)

	// Act
	err := s.HandOffDetachedMerge(context.Background(), repo, requester.ID, request, forked.ID, "sample-merge", "", time.Unix(200, 0))

	// Assert
	if err != nil {
		t.Fatalf("HandOffDetachedMerge: %v", err)
	}
	queue, err := s.MergeQueue(context.Background(), repo)
	if err != nil || len(queue) != 1 || !queue[0].Source.Front {
		t.Fatalf("MergeQueue = %+v, %v; want the merge workspace's entry carrying Front", queue, err)
	}
}

func TestHandOffDetachedMergeRefusesWithNothingChanged(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(t *testing.T, s *store, repo RepoKey, requester WorkspaceID) DetachedMergeID
		wantErr error
	}{
		{name: "no request", wantErr: ErrNotFound, arrange: func(t *testing.T, s *store, repo RepoKey, requester WorkspaceID) DetachedMergeID {
			return NewDetachedMergeID()
		}},
		{name: "another request", arrange: func(t *testing.T, s *store, repo RepoKey, requester WorkspaceID) DetachedMergeID {
			requestDetached(t, s, repo, requester, NewDetachedMergeID())
			return NewDetachedMergeID()
		}},
		{name: "an in-place request", arrange: func(t *testing.T, s *store, repo RepoKey, requester WorkspaceID) DetachedMergeID {
			if err := s.RequestMerge(context.Background(), repo, requester, MergeSource{Kind: MergeSourceOwnBranch}, time.Unix(100, 0)); err != nil {
				t.Fatalf("RequestMerge: %v", err)
			}
			return NewDetachedMergeID()
		}},
		{name: "another requester's request", arrange: func(t *testing.T, s *store, repo RepoKey, requester WorkspaceID) DetachedMergeID {
			request := NewDetachedMergeID()
			requestDetached(t, s, repo, testWorkspace(t, s).ID, request)
			return request
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			requester := testWorkspace(t, s)
			repo := RepoKey(t.TempDir())
			request := tt.arrange(t, s, repo, requester.ID)
			forked := registerMergeWorkspace(t, s, requester.ID, request)
			before, err := s.MergeQueue(context.Background(), repo)
			if err != nil {
				t.Fatalf("MergeQueue: %v", err)
			}
			requestsBefore, err := s.AllAsyncMergeRequests(context.Background())
			if err != nil {
				t.Fatalf("AllAsyncMergeRequests: %v", err)
			}

			// Act
			err = s.HandOffDetachedMerge(context.Background(), repo, requester.ID, request, forked.ID, "sample-merge", "", time.Unix(200, 0))

			// Assert: refused, and the queue and the requests are as they were.
			if err == nil || (tt.wantErr != nil && !errors.Is(err, tt.wantErr)) {
				t.Fatalf("HandOffDetachedMerge = %v, want a refusal", err)
			}
			after, err := s.MergeQueue(context.Background(), repo)
			if err != nil || len(after) != len(before) {
				t.Fatalf("queue after = %+v, %v; want unchanged %+v", after, err, before)
			}
			requestsAfter, err := s.AllAsyncMergeRequests(context.Background())
			if err != nil || len(requestsAfter) != len(requestsBefore) {
				t.Fatalf("requests after = %+v, %v; want unchanged %+v", requestsAfter, err, requestsBefore)
			}
		})
	}
}

func TestMergeWorkspaceRemovedAtRoundTripsAndKeepsTheFirstInstant(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	first := time.Unix(300, 0).UTC()
	if err := s.RecordMergeWorkspaceRemoved(context.Background(), "gone", "requester", first); err != nil {
		t.Fatalf("RecordMergeWorkspaceRemoved: %v", err)
	}

	// Act
	if err := s.RecordMergeWorkspaceRemoved(context.Background(), "gone", "requester", time.Unix(400, 0)); err != nil {
		t.Fatalf("RecordMergeWorkspaceRemoved again: %v", err)
	}
	at, found, err := s.MergeWorkspaceRemovedAt(context.Background(), "gone")

	// Assert
	if err != nil || !found || !at.Equal(first) {
		t.Fatalf("MergeWorkspaceRemovedAt = %v, %v, %v; want %v", at, found, err, first)
	}
}

func TestMergeWorkspaceRemovedAtAnswersNoneForAWorkspaceNeverRemoved(t *testing.T) {
	// Arrange
	s, _ := testStore(t)

	// Act
	_, found, err := s.MergeWorkspaceRemovedAt(context.Background(), "present")

	// Assert
	if err != nil || found {
		t.Fatalf("MergeWorkspaceRemovedAt = %v, %v; want none", found, err)
	}
}

func TestHandOffDetachedMergeCarriesABehindRequestToTheMergeWorkspace(t *testing.T) {
	// Arrange: a detached merge asked to go behind another merge.
	s, _ := testStore(t)
	requester := testWorkspace(t, s)
	repo := RepoKey(t.TempDir())
	request := NewDetachedMergeID()
	asyncRequest(t, s, repo, requester.ID, MergeSource{Kind: MergeSourceBranch, Branch: "feature", Detach: request, DetachMaker: "maker", DetachName: "sample-merge", Behind: "ws-ahead", BehindName: "ahead"})
	forked := registerMergeWorkspace(t, s, requester.ID, request)

	// Act
	err := s.HandOffDetachedMerge(context.Background(), repo, requester.ID, request, forked.ID, "sample-merge", "", time.Unix(200, 0))

	// Assert
	if err != nil {
		t.Fatalf("HandOffDetachedMerge: %v", err)
	}
	queue, err := s.MergeQueue(context.Background(), repo)
	if err != nil || len(queue) != 1 || queue[0].Source.Behind != "ws-ahead" {
		t.Fatalf("MergeQueue = %+v, %v; want the merge workspace's entry carrying Behind", queue, err)
	}
}

func TestHandOffDetachedMergeMovesTheRequestsWaitOntoTheMergeWorkspace(t *testing.T) {
	// Arrange: the detached request waits on a merge ahead, and another
	// request waits on the detached request.
	s, _ := testStore(t)
	requester, ahead, later := testWorkspace(t, s), testWorkspace(t, s), testWorkspace(t, s)
	repo := RepoKey(t.TempDir())
	queued(t, s, repo, ahead.ID, time.Unix(50, 0))
	request := NewDetachedMergeID()
	asyncRequest(t, s, repo, requester.ID, MergeSource{Kind: MergeSourceBranch, Branch: "feature", Detach: request, DetachMaker: "maker", DetachName: "sample-merge", Behind: ahead.ID, BehindName: "ahead"})
	if err := s.RequestMerge(context.Background(), repo, later.ID, MergeSource{Kind: MergeSourceOwnBranch, Behind: WorkspaceID(request), BehindName: "sample-merge"}, time.Unix(150, 0)); err != nil {
		t.Fatalf("RequestMerge(later): %v", err)
	}
	forked := registerMergeWorkspace(t, s, requester.ID, request)

	// Act
	err := s.HandOffDetachedMerge(context.Background(), repo, requester.ID, request, forked.ID, "sample-merge", "", time.Unix(200, 0))

	// Assert
	if err != nil {
		t.Fatalf("HandOffDetachedMerge: %v", err)
	}
	if entry := entryNamed(t, s, repo, forked.ID); entry.WaitsOn != ahead.ID || entry.WaitsOnName != "ahead" || !entry.HeldBehind() {
		t.Fatalf("merge workspace's entry = %+v, want it waiting on %s", entry, ahead.ID)
	}
	if entry := entryNamed(t, s, repo, later.ID); entry.WaitsOn != forked.ID || entry.Source.Behind != forked.ID || !entry.HeldBehind() {
		t.Fatalf("later entry = %+v, want it placed behind and waiting on the merge workspace %s", entry, forked.ID)
	}
}
