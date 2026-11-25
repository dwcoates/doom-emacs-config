package daemonrules

import (
	"os"
	"strings"
	"testing"

	"agent-repld/reposcans/internal/sourcescan"
)

// internalAtomicfileDir is the package these rules hold, under the daemon module's root.
const internalAtomicfileDir = "internal/atomicfile"

// TestEverySiteReplacesThroughThisPackage holds the daemon's whole-file
// replaces to Replace: a site that hand-rolls its own temporary fails here.
func TestEverySiteReplacesThroughThisPackage(t *testing.T) {
	sourcescan.InPackage(t, internalAtomicfileDir)
	// Arrange.
	sites := []string{
		"../daemonaddr/claim.go",
		"../rollout/carry.go",
		"../rollout/manifest.go",
		"../rollout/spawn.go",
		"../classifierupdate/update.go",
	}
	for _, site := range sites {
		t.Run(site, func(t *testing.T) {
			// Act.
			src, err := os.ReadFile(site)
			if err != nil {
				t.Fatalf("read %s: %v", site, err)
			}

			// Assert.
			if strings.Contains(string(src), "os.CreateTemp(") || !strings.Contains(string(src), "atomicfile.Replace(") {
				t.Fatalf("%s replaces a file without atomicfile.Replace", site)
			}
		})
	}
}
