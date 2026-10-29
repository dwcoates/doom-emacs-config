package wsm

import (
	"context"
	"testing"
)

func TestWorkspaceBook(t *testing.T) {
	tests := []struct {
		name      string
		set       []string
		wantBook  string
		wantFound bool
	}{
		{name: "a workspace no shim has stated a book for has none"},
		{name: "a stated book is read back", set: []string{"agent-original"}, wantBook: "agent-original", wantFound: true},
		{name: "a later statement replaces it", set: []string{"agent-original", "agent-rebound"}, wantBook: "agent-rebound", wantFound: true},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			s, _ := testStore(t)
			ws := testWorkspace(t, s).ID
			for _, book := range tt.set {
				if err := s.SetWorkspaceBook(context.Background(), ws, book); err != nil {
					t.Fatalf("SetWorkspaceBook: %v", err)
				}
			}

			// Act
			book, found, err := s.WorkspaceBook(context.Background(), ws)

			// Assert
			if err != nil {
				t.Fatalf("WorkspaceBook: %v", err)
			}
			if book != tt.wantBook || found != tt.wantFound {
				t.Fatalf("WorkspaceBook = %q, %v; want %q, %v", book, found, tt.wantBook, tt.wantFound)
			}
		})
	}
}

func TestSetWorkspaceBookRefusesAnEmptyBook(t *testing.T) {
	// Arrange
	s, _ := testStore(t)
	ws := testWorkspace(t, s).ID

	// Act
	err := s.SetWorkspaceBook(context.Background(), ws, "")

	// Assert
	if err == nil {
		t.Fatal("SetWorkspaceBook with no book succeeded, want a refusal")
	}
}
