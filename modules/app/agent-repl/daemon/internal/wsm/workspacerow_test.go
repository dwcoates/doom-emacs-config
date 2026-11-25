package wsm

import (
	"context"
	"errors"
	"testing"
)

func TestScanWorkspaceRowReadsTheNamedColumns(t *testing.T) {
	// Arrange.
	s, _ := testStore(t)
	ws := testWorkspaceNamed(t, s, "scanned")

	// Act.
	var (
		name   string
		closed bool
	)
	err := scanWorkspaceRow(context.Background(), s.db(), ws.ID, "name, closed", &name, &closed)

	// Assert.
	if err != nil {
		t.Fatalf("scanWorkspaceRow: %v", err)
	}
	if name != "scanned" || closed {
		t.Fatalf("read (%q, %v), want (\"scanned\", false)", name, closed)
	}
}

func TestScanWorkspaceRowRefusesAnUnknownWorkspace(t *testing.T) {
	// Arrange.
	s, _ := testStore(t)

	// Act.
	var name string
	err := scanWorkspaceRow(context.Background(), s.db(), "ws-missing", "name", &name)

	// Assert.
	if !errors.Is(err, ErrNotFound) {
		t.Fatalf("scanWorkspaceRow = %v, want ErrNotFound", err)
	}
}
