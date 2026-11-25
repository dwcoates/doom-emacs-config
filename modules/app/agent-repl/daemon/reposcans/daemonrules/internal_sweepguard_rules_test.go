package daemonrules

import (
	"io/fs"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"agent-repld/reposcans/internal/sourcescan"
)

// internalSweepguardDir is the package these rules hold, under the daemon module's root.
const internalSweepguardDir = "internal/sweepguard"

// TestNoSweepHandRollsItsOwnGuard holds every daemon source to this guard: a
// file that both TryLocks a mutex and takes a record lock is a sweep guarding
// itself by hand, which drifts from the records and refusals here.
func TestNoSweepHandRollsItsOwnGuard(t *testing.T) {
	sourcescan.InPackage(t, internalSweepguardDir)
	// Arrange: a test runs in its package's directory.
	here, err := os.Getwd()
	if err != nil {
		t.Fatalf("the working directory, which is this package's: %v", err)
	}
	internal := filepath.Join(here, "..")
	var offenders []string

	// Act
	err = filepath.WalkDir(internal, func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if d.IsDir() || !strings.HasSuffix(path, ".go") || strings.HasSuffix(path, "_test.go") || filepath.Dir(path) == here {
			return nil
		}
		b, err := os.ReadFile(path)
		if err != nil {
			return err
		}
		if strings.Contains(string(b), ".TryLock()") && strings.Contains(string(b), "recordlock.Hold(") {
			rel, _ := filepath.Rel(internal, path)
			offenders = append(offenders, rel)
		}
		return nil
	})

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	if len(offenders) > 0 {
		t.Fatalf("these sources guard a sweep by hand; use sweepguard.Guard:\n%s", strings.Join(offenders, "\n"))
	}
}
