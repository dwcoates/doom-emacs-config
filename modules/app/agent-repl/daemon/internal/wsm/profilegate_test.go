package wsm

import (
	"context"
	"testing"
	"time"
)

func TestARequestKeepsItsProfileGateInTheQueue(t *testing.T) {
	for _, profile := range []bool{true, false} {
		t.Run(map[bool]string{true: "a profiled gate", false: "an ordinary gate"}[profile], func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			ws := testWorkspace(t, s)
			repo := RepoKey(t.TempDir())

			// Act
			err := s.RequestMerge(context.Background(), repo, ws.ID, MergeSource{Kind: MergeSourceOwnBranch, ProfileGate: profile}, time.Unix(100, 0))
			queue, readErr := s.MergeQueue(context.Background(), repo)

			// Assert
			if err != nil || readErr != nil || len(queue) != 1 {
				t.Fatalf("RequestMerge = %v, MergeQueue = %+v, %v; want one request", err, queue, readErr)
			}
			if queue[0].Source.ProfileGate != profile {
				t.Fatalf("ProfileGate = %v, want %v", queue[0].Source.ProfileGate, profile)
			}
		})
	}
}

func TestAnAsyncRequestKeepsItsProfileGate(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	requester := testWorkspace(t, s)
	repo := RepoKey(t.TempDir())

	// Act
	asyncRequest(t, s, repo, requester.ID, MergeSource{Kind: MergeSourceBranch, Branch: "feature", Detach: NewDetachedMergeID(), DetachMaker: "maker", DetachName: "sample-merge", ProfileGate: true})

	// Assert
	if requests := asyncRequestsOf(t, s, requester.ID); len(requests) != 1 || !requests[0].Source.ProfileGate {
		t.Fatalf("requests = %+v, want one asking for its gate profiled", requests)
	}
}

func TestHandOffDetachedMergeCarriesTheProfileGateToTheMergeWorkspace(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	requester := testWorkspace(t, s)
	repo := RepoKey(t.TempDir())
	request := NewDetachedMergeID()
	asyncRequest(t, s, repo, requester.ID, MergeSource{Kind: MergeSourceBranch, Branch: "feature", Detach: request, DetachMaker: "maker", DetachName: "sample-merge", ProfileGate: true})
	forked := registerMergeWorkspace(t, s, requester.ID, request)

	// Act
	err := s.HandOffDetachedMerge(context.Background(), repo, requester.ID, request, forked.ID, "sample-merge", "", time.Unix(200, 0))

	// Assert
	if err != nil {
		t.Fatalf("HandOffDetachedMerge: %v", err)
	}
	queue, err := s.MergeQueue(context.Background(), repo)
	if err != nil || len(queue) != 1 || queue[0].Workspace != forked.ID || !queue[0].Source.ProfileGate {
		t.Fatalf("MergeQueue = %+v, %v; want the merge workspace's entry asking for its gate profiled", queue, err)
	}
}
