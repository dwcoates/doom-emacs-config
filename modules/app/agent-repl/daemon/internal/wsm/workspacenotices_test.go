package wsm

import (
	"context"
	"testing"
)

func TestClosingAWorkspaceEndsItsUntoldWorkspaceNotices(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	closing := testWorkspace(t, s)
	other := testWorkspaceNamed(t, s, "other")
	followed := testWorkspaceNamed(t, s, "followed")
	for _, ws := range []WorkspaceID{closing.ID, other.ID} {
		if err := s.RecordUntoldWorkspaceNotice(context.Background(), UntoldWorkspaceNotice{Subscriber: ws, Followed: followed.ID, Turn: "turn-1", Sentences: []string{"one."}}); err != nil {
			t.Fatal(err)
		}
	}

	// Act
	if err := s.MarkClosed(context.Background(), closing.ID, Closure{At: instant, By: CloseByUser}); err != nil {
		t.Fatalf("MarkClosed: %v", err)
	}

	// Assert
	_, closingFound, err := s.UntoldWorkspaceNotice(context.Background(), closing.ID, followed.ID)
	if err != nil {
		t.Fatal(err)
	}
	_, otherFound, err := s.UntoldWorkspaceNotice(context.Background(), other.ID, followed.ID)
	if err != nil {
		t.Fatal(err)
	}
	if closingFound || !otherFound {
		t.Fatalf("untold after the close: closing %v, other %v; want only the other's kept", closingFound, otherFound)
	}
}
