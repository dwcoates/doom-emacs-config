package daemonrules

import (
	"os"
	"strings"
	"testing"

	"agent-repld/reposcans/internal/sourcescan"
)

// internalUsersetupDir is the package these rules hold, under the daemon module's root.
const internalUsersetupDir = "internal/usersetup"

// TestPairsIsTheOnePairParser pins that every per-user pair variable is read
// through Pairs: a reader that splits its own pairs would drift from the
// shared error and trimming rules.
func TestPairsIsTheOnePairParser(t *testing.T) {
	sourcescan.InPackage(t, internalUsersetupDir)
	for _, path := range []string{"../pullrefs/pullrefs.go", "../externalbrowser/slack.go"} {
		t.Run(path, func(t *testing.T) {
			// Arrange.
			src, err := os.ReadFile(path) //nolint:gosec // repository source
			if err != nil {
				t.Fatal(err)
			}
			body := string(src)

			// Assert.
			if !strings.Contains(body, "usersetup.Pairs(") {
				t.Fatalf("%s does not read its pairs through usersetup.Pairs", path)
			}
			if strings.Contains(body, `strings.Cut(`) && strings.Contains(body, `"="`) {
				t.Fatalf("%s splits `<key>=<value>` pairs by hand", path)
			}
		})
	}
}
