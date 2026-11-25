package daemonrules

import (
	"os"
	"path/filepath"
	"strings"
	"testing"

	"agent-repld/reposcans/internal/sourcescan"
)

// internalVocabDir is the package these rules hold, under the daemon module's root.
const internalVocabDir = "internal/vocab"

// THE RESOLVERS READ THEIR ARMS FROM THE ONE READER: no production file keeps
// a hand-typed copy of a status oneof for the vocabulary to be asserted
// against (vocab.RosterStatusArms, vocab.FooterStatusArms). A production file
// declaring one by the old name fails here.
func TestNoResolverKeepsAHandTypedStatusArmList(t *testing.T) {
	sourcescan.InPackage(t, internalVocabDir)
	// Arrange.
	root := "../.."
	var found []string

	// Act.
	err := filepath.WalkDir(root, func(path string, d os.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if d.IsDir() || filepath.Ext(path) != ".go" || strings.HasSuffix(path, "_test.go") {
			return nil
		}
		raw, err := os.ReadFile(path)
		if err != nil {
			return err
		}
		for _, name := range []string{"statusArms", "mergeArms"} {
			if strings.Contains(string(raw), "var "+name+" = []string{") {
				found = append(found, path)
			}
		}
		return nil
	})

	// Assert.
	if err != nil {
		t.Fatalf("walking the module: %v", err)
	}
	if len(found) != 0 {
		t.Fatalf("hand-typed status arm lists in %v, want every resolver on the vocab readers", found)
	}
}

// TestEveryMarkSectionChecksItsMotionThroughCheckMotion fails a mark section
// whose motion is checked by hand rather than through the one check.
func TestEveryMarkSectionChecksItsMotionThroughCheckMotion(t *testing.T) {
	sourcescan.InPackage(t, internalVocabDir)
	// Arrange.
	raw, err := os.ReadFile("api.go")
	if err != nil {
		t.Fatalf("read api.go: %v", err)
	}

	// Act.
	handRolled := strings.Count(string(raw), "contains(markMotions,")

	// Assert: the one membership test is checkMotion's own.
	if handRolled != 1 {
		t.Fatalf("api.go tests markMotions membership %d times, want once, inside checkMotion", handRolled)
	}
}
