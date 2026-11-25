package daemonrules

import (
	"fmt"
	"io/fs"
	"os"
	"path/filepath"
	"regexp"
	"strings"
	"testing"

	"agent-repld/reposcans/internal/sourcescan"
)

// internalLatestmodelDir is the package these rules hold, under the daemon module's root.
const internalLatestmodelDir = "internal/latestmodel"

// activityModelRead is a resolver reading an activity's model; every such
// read must hand it straight to a Ledger.
var activityModelRead = regexp.MustCompile(`act\.(GetModel\(\)|Model\b)`)

func TestNoDaemonSiteReadsAnActivitysModelOutsideTheLedger(t *testing.T) {
	sourcescan.InPackage(t, internalLatestmodelDir)
	// Arrange: every production source of the daemon's resolvers.
	root := filepath.Join("..", "resolve")
	var offenders []string

	// Act.
	err := filepath.WalkDir(root, func(path string, d fs.DirEntry, err error) error {
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
		for i, line := range strings.Split(string(src), "\n") {
			if activityModelRead.MatchString(line) && !strings.Contains(line, ".Record(") {
				offenders = append(offenders, fmt.Sprintf("%s:%d", path, i+1))
			}
		}
		return nil
	})

	// Assert: a subagent's model is filed in one place, Ledger.Record.
	if err != nil {
		t.Fatalf("walk: %v", err)
	}
	if len(offenders) > 0 {
		t.Fatalf("hand-rolled activity model reads (use latestmodel.Ledger): %s", strings.Join(offenders, ", "))
	}
}
