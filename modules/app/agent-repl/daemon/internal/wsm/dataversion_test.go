package wsm

import (
	"context"
	"testing"
)

func TestDataVersionChangesWhenAnotherHandleCommits(t *testing.T) {
	// Arrange
	s, _ := fileStore(t)
	peerHandle, err := Open(context.Background(), s.path, WithUnsyncedWrites())
	if err != nil {
		t.Fatalf("Open the peer handle: %v", err)
	}
	t.Cleanup(func() { peerHandle.Close() })
	peer := peerHandle.(*store)
	ws := testWorkspace(t, peer)
	before, err := s.DataVersion(context.Background())
	if err != nil {
		t.Fatalf("DataVersion before: %v", err)
	}

	// Act
	if err := peer.SetCurrent(context.Background(), ws.ID, instant); err != nil {
		t.Fatalf("peer SetCurrent: %v", err)
	}

	// Assert
	after, err := s.DataVersion(context.Background())
	if err != nil {
		t.Fatalf("DataVersion after: %v", err)
	}
	if after == before {
		t.Fatalf("data version stayed %d across the peer's commit, want a change", before)
	}
}

func TestDataVersionStaysPutForTheHandlesOwnCommit(t *testing.T) {
	// Arrange
	s, _ := fileStore(t)
	ws := testWorkspace(t, s)
	before, err := s.DataVersion(context.Background())
	if err != nil {
		t.Fatalf("DataVersion before: %v", err)
	}

	// Act
	if err := s.SetCurrent(context.Background(), ws.ID, instant); err != nil {
		t.Fatalf("SetCurrent: %v", err)
	}

	// Assert
	after, err := s.DataVersion(context.Background())
	if err != nil {
		t.Fatalf("DataVersion after: %v", err)
	}
	if after != before {
		t.Fatalf("data version moved %d -> %d on the handle's own commit, want it unchanged", before, after)
	}
}
