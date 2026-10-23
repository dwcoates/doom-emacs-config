package childproc

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// TestTheSourceScansShareOneWalk holds this package's scans to
// sourcescan.EachDaemonSource: a scan with a walk of its own would drift in
// what it skips.
func TestTheSourceScansShareOneWalk(t *testing.T) {
	// Arrange
	matches, err := filepath.Glob("*_test.go")
	if err != nil {
		t.Fatalf("glob: %v", err)
	}
	var offenders []string

	// Act
	for _, name := range matches {
		// This file names the patterns it looks for.
		if name == "sources_test.go" {
			continue
		}
		raw, err := os.ReadFile(name)
		if err != nil {
			t.Fatalf("read %s: %v", name, err)
		}
		if strings.Contains(string(raw), "filepath.WalkDir(") || strings.Contains(string(raw), "parser.ParseFile(") {
			offenders = append(offenders, name)
		}
	}

	// Assert
	if len(offenders) > 0 {
		t.Fatalf("these files walk the daemon sources themselves; use sourcescan.EachDaemonSource: %v", offenders)
	}
}
