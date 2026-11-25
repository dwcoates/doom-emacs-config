package daemonrules

import (
	"io/fs"
	"os"
	"path/filepath"
	"regexp"
	"strings"
	"testing"

	"agent-repld/reposcans/internal/sourcescan"
)

// internalBounceDir is the package these rules hold, under the daemon module's root.
const internalBounceDir = "internal/bounce"

// TestDoneOutcomesAreClassifiedOnlyThroughOutcomeOf pins that no consumer of a
// bounce's Done hand-rolls its own reading of the non-failure sentinels: a
// consumer that tests one and forgets the other records an ordinary outcome as
// a failure (the handed-across bounce, recorded at ERROR on the 2026-09-29
// deploy). The registry that PRODUCES the sentinels is exempt.
func TestDoneOutcomesAreClassifiedOnlyThroughOutcomeOf(t *testing.T) {
	sourcescan.InPackage(t, internalBounceDir)
	// Arrange.
	root := filepath.Join("..", "..")
	handRolled := regexp.MustCompile(`errors\.Is\([^)]*bounce\.Err(Unregistered|HandedAcross|Deferred)\)`)
	exempt := filepath.Join("internal", "promptqueue") + string(os.PathSeparator)
	var offenders []string

	// Act.
	err := filepath.WalkDir(root, func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if d.IsDir() || !strings.HasSuffix(path, ".go") || strings.HasSuffix(path, "_test.go") {
			return nil
		}
		rel, _ := filepath.Rel(root, path)
		if strings.HasPrefix(rel, exempt) {
			return nil
		}
		src, err := os.ReadFile(path)
		if err != nil {
			return err
		}
		if handRolled.Match(src) {
			offenders = append(offenders, rel)
		}
		return nil
	})

	// Assert.
	if err != nil {
		t.Fatalf("walking the daemon's sources: %v", err)
	}
	if len(offenders) > 0 {
		t.Fatalf("these files classify a bounce's Done by hand instead of through bounce.OutcomeOf: %v", offenders)
	}
}
