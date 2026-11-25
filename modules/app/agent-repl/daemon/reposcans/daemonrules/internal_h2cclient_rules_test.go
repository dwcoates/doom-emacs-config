package daemonrules

import (
	"os"
	"path/filepath"
	"strings"
	"testing"

	"agent-repld/reposcans/internal/sourcescan"
)

// internalH2cclientDir is the package these rules hold, under the daemon module's root.
const internalH2cclientDir = "internal/h2cclient"

// TestNoOtherDaemonPackageBuildsAnH2CTransport pins the extraction: every
// daemon client of an h2c Connect peer goes through this package, so the
// dialing can never drift between them.
func TestNoOtherDaemonPackageBuildsAnH2CTransport(t *testing.T) {
	sourcescan.InPackage(t, internalH2cclientDir)
	// Arrange
	root := filepath.Join("..", "..")
	found := []string{}

	// Act
	err := filepath.WalkDir(root, func(path string, d os.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if d.IsDir() || filepath.Ext(path) != ".go" || strings.HasSuffix(path, "_test.go") || strings.Contains(path, "internal/h2cclient/") {
			return nil
		}
		body, err := os.ReadFile(path)
		if err != nil {
			return err
		}
		if strings.Contains(string(body), "http2.Transport{") {
			found = append(found, path)
		}
		return nil
	})

	// Assert
	if err != nil {
		t.Fatal(err)
	}
	if len(found) != 0 {
		t.Errorf("these files build their own h2c transport; use h2cclient: %v", found)
	}
}
