package daemonrules

import (
	"os"
	"path/filepath"
	"regexp"
	"strings"
	"testing"

	"agent-repld/reposcans/internal/sourcescan"
)

// internalEnvcDir is the package these rules hold, under the daemon module's root.
const internalEnvcDir = "internal/envc"

// handRolledTestRunRefusal is a test-run seam's refusal spelled out by hand
// instead of through RequireTestRun.
var handRolledTestRunRefusal = regexp.MustCompile(`is a test-run seam and is honored only with`)

// TestEveryTestRunSeamRefusesThroughRequireTestRun keeps every seam's boot
// refusal on the one guard, so a hand-rolled one cannot drift from it.
func TestEveryTestRunSeamRefusesThroughRequireTestRun(t *testing.T) {
	sourcescan.InPackage(t, internalEnvcDir)
	// Arrange
	root := filepath.Join("..", "..")
	var offenders []string

	// Act
	err := filepath.WalkDir(root, func(path string, entry os.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if entry.IsDir() || filepath.Ext(path) != ".go" || filepath.Base(path) == "testrun.go" ||
			strings.HasSuffix(path, "_test.go") {
			return nil
		}
		src, err := os.ReadFile(path)
		if err != nil {
			return err
		}
		if handRolledTestRunRefusal.Match(src) {
			offenders = append(offenders, path)
		}
		return nil
	})

	// Assert
	if err != nil {
		t.Fatalf("walk the daemon's sources: %v", err)
	}
	if len(offenders) != 0 {
		t.Fatalf("%v hand-roll a test-run seam's refusal; call envc.Contracts.RequireTestRun", offenders)
	}
}
