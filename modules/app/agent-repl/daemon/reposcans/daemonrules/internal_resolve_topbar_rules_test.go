package daemonrules

import (
	"os"
	"strings"
	"testing"

	"agent-repld/reposcans/internal/sourcescan"
)

// internalResolveTopbarDir is the package these rules hold, under the daemon module's root.
const internalResolveTopbarDir = "internal/resolve/topbar"

func TestEverySelectionsErrorGoesThroughTheHelper(t *testing.T) {
	sourcescan.InPackage(t, internalResolveTopbarDir)
	// Arrange
	src, err := os.ReadFile("selections.go")
	if err != nil {
		t.Fatalf("read selections.go: %v", err)
	}

	// Act
	direct := strings.Count(string(src), "log.Error(opSelections") +
		strings.Count(string(src), "log.Log(dlog.LevelError, opSelections")

	// Assert: the only direct record is the helper's own.
	if direct != 1 || strings.Count(string(src), "r.selectionsError(ws, ") < 3 {
		t.Fatalf("selections.go has %d direct opSelections records, want only selectionsError's", direct)
	}
}
