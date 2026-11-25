package daemonrules

import (
	"io/fs"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"agent-repld/reposcans/internal/sourcescan"
)

// internalEffortlevelDir is the package these rules hold, under the daemon module's root.
const internalEffortlevelDir = "internal/effortlevel"

// TestNoOtherPackageSpellsALevel asserts the call sites share this table: a
// production file elsewhere naming a level's enum value by hand is a second
// table drifting beside this one.
func TestNoOtherPackageSpellsALevel(t *testing.T) {
	sourcescan.InPackage(t, internalEffortlevelDir)
	// Arrange.
	root := filepath.Join("..")
	var offenders []string

	// Act.
	err := filepath.WalkDir(root, func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if d.IsDir() || !strings.HasSuffix(path, ".go") || strings.HasSuffix(path, "_test.go") {
			return nil
		}
		if filepath.Base(filepath.Dir(path)) == "effortlevel" {
			return nil
		}
		body, err := os.ReadFile(path)
		if err != nil {
			return err
		}
		if strings.Contains(string(body), "AGENT_EFFORT_LEVEL_XHIGH") {
			offenders = append(offenders, path)
		}
		return nil
	})

	// Assert.
	if err != nil {
		t.Fatalf("walk: %v", err)
	}
	if len(offenders) != 0 {
		t.Errorf("files spelling a level outside effortlevel: %v", offenders)
	}
}
