package wsm

import (
	"context"
	"errors"
	"regexp"
	"testing"

	"claude-repld/internal/sourcescan"
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

// TestEveryByIdColumnReadGoesThroughScanWorkspaceRow pins that no store source
// hand-rolls a by-id workspace read, whose not-found mapping would drift.
func TestEveryByIdColumnReadGoesThroughScanWorkspaceRow(t *testing.T) {
	// Arrange.
	handRolled := regexp.MustCompile("FROM workspaces WHERE id = \\?`, \\w+\\)\\.Scan\\(")
	var offenders []string

	// Act.
	for _, file := range sourcescan.Production(t) {
		// workspacerow.go IS the helper. faults.go's existence check names the
		// FAULT's workspace in its refusal, a different answer from a
		// workspace read's.
		if file.Name == "workspacerow.go" || file.Name == "faults.go" {
			continue
		}
		if handRolled.Match(file.Source) {
			offenders = append(offenders, file.Name)
		}
	}

	// Assert.
	if len(offenders) > 0 {
		t.Fatalf("these files read workspace columns by id by hand; use scanWorkspaceRow: %v", offenders)
	}
}
