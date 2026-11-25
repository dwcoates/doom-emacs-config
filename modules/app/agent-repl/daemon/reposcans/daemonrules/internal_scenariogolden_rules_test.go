package daemonrules

import (
	"os"
	"strings"
	"testing"

	"agent-repld/reposcans/internal/sourcescan"
)

// internalScenariogoldenDir is the package these rules hold, under the daemon module's root.
const internalScenariogoldenDir = "internal/scenariogolden"

func TestTheScenarioGoldensWalkJSONOnlyThroughMapJSONLeaves(t *testing.T) {
	sourcescan.InPackage(t, internalScenariogoldenDir)
	// A hand-rolled walk drifts from the shared one's sorted order and drops;
	// only mapJSONLeaves may recurse over a decoded value here.
	src, err := os.ReadFile("golden.go")
	if err != nil {
		t.Fatal(err)
	}
	if n := strings.Count(string(src), "\n\tvar walk func("); n != 1 {
		t.Fatalf("golden.go declares %d JSON walks, want one, inside mapJSONLeaves", n)
	}
}
