package daemonrules

import (
	"io/fs"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"agent-repld/reposcans/internal/sourcescan"
)

// internalCommandcatalogCatalogtestDir is the package these rules hold, under the daemon module's root.
const internalCommandcatalogCatalogtestDir = "internal/commandcatalog/catalogtest"

// TestNoTestHandRollsANamedCatalog pins that every test builds its
// names-only catalogs through Named, so a second fixture cannot drift from it.
func TestNoTestHandRollsANamedCatalog(t *testing.T) {
	sourcescan.InPackage(t, internalCommandcatalogCatalogtestDir)
	// Arrange.
	root := filepath.Join("..", "..")
	var offenders []string

	// Act.
	err := filepath.WalkDir(root, func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if d.IsDir() || !strings.HasSuffix(path, "_test.go") || strings.Contains(path, "catalogtest") {
			return nil
		}
		raw, err := os.ReadFile(path)
		if err != nil {
			return err
		}
		if strings.Contains(string(raw), "SessionCommandCatalogEntry{Name: name}") {
			offenders = append(offenders, path)
		}
		return nil
	})

	// Assert.
	if err != nil {
		t.Fatalf("walk the daemon's tests: %v", err)
	}
	if len(offenders) != 0 {
		t.Fatalf("tests hand-roll a names-only catalog instead of catalogtest.Named: %v", offenders)
	}
}
