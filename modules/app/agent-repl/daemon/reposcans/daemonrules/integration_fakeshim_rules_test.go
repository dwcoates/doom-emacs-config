package daemonrules

import (
	"os"
	"strings"
	"testing"

	"agent-repld/reposcans/internal/sourcescan"
)

// integrationFakeshimDir is the package these rules hold, under the daemon module's root.
const integrationFakeshimDir = "integration/fakeshim"

// TestFakeTranscriptWritesShareOneHelper fails when a fake RPC writes a
// transcript line by hand instead of through appendTranscriptLine.
func TestFakeTranscriptWritesShareOneHelper(t *testing.T) {
	sourcescan.InPackage(t, integrationFakeshimDir)
	// Arrange
	source, err := os.ReadFile("server.go")
	if err != nil {
		t.Fatal(err)
	}

	// Act
	writes := strings.Count(string(source), "os.WriteFile(path") + strings.Count(string(source), "os.OpenFile(path")

	// Assert
	if writes != 1 {
		t.Errorf("server.go opens or writes a transcript path %d times, want once, in appendTranscriptLine", writes)
	}
}
