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

// internalTokenusageDir is the package these rules hold, under the daemon module's root.
const internalTokenusageDir = "internal/tokenusage"

// TestResolversReconcileOnlyThroughThisPackage enforces that usage
// reconciliation has ONE home: no resolver compares usages field by field or
// spells a contradiction text itself, so the rules cannot drift apart between
// the topbar and the footer again.
func TestResolversReconcileOnlyThroughThisPackage(t *testing.T) {
	sourcescan.InPackage(t, internalTokenusageDir)
	// Arrange: every production source of the daemon's internal packages outside tokenusage.
	root := ".."
	forbidden := []struct {
		name    string
		pattern *regexp.Regexp
	}{
		{"a usage getter compared by hand", regexp.MustCompile(`Get(OutputTokens|OutputThinkingTokens|Read|Written|Unwritten|Provisional)\(\)\s*(==|!=|>|<)`)},
		{"a thinking share compared by hand", regexp.MustCompile(`\.thinking\s*(==|!=|>|<)`)},
		{"the two-usages contradiction text", regexp.MustCompile(`two different usages`)},
		{"the thinking contradiction text", regexp.MustCompile(`more thinking tokens`)},
	}
	var offenses []string

	// Act
	err := filepath.WalkDir(root, func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if d.IsDir() || !strings.HasSuffix(path, ".go") || strings.HasSuffix(path, "_test.go") {
			return nil
		}
		if filepath.Base(filepath.Dir(path)) == "tokenusage" {
			return nil
		}
		body, err := os.ReadFile(path)
		if err != nil {
			return err
		}
		for i, line := range strings.Split(string(body), "\n") {
			for _, f := range forbidden {
				if f.pattern.MatchString(line) {
					offenses = append(offenses, fmt.Sprintf("%s:%d: %s: %s", path, i+1, f.name, strings.TrimSpace(line)))
				}
			}
		}
		return nil
	})

	// Assert
	if err != nil {
		t.Fatalf("walking %s: %v", root, err)
	}
	if len(offenses) > 0 {
		t.Fatalf("usage reconciliation done outside package tokenusage:\n%s", strings.Join(offenses, "\n"))
	}
}
