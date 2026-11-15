package procgroup

import (
	"os"
	"path/filepath"
	"strings"
	"testing"

	"agent-repld/internal/sourcescan"

	"agentrepl/logging/procstat"
)

// TestEveryDaemonProcFileIsParsedByProcstat holds the daemon's sources to
// procstat.HandParsed: every /proc read goes through agentrepl/logging/procstat.
func TestEveryDaemonProcFileIsParsedByProcstat(t *testing.T) {
	// Arrange
	root := sourcescan.DaemonRoot(t)
	var offenders []string

	// Act
	err := filepath.WalkDir(root, func(path string, d os.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if d.IsDir() || !strings.HasSuffix(path, ".go") || strings.HasSuffix(path, "_test.go") {
			return nil
		}
		src, err := os.ReadFile(path)
		if err != nil {
			return err
		}
		if procstat.HandParsed(string(src)) {
			offenders = append(offenders, path)
		}
		return nil
	})

	// Assert
	if err != nil {
		t.Fatalf("walk the daemon module: %v", err)
	}
	if len(offenders) > 0 {
		t.Fatalf("%v parse a /proc file by hand instead of through procstat", offenders)
	}
}
