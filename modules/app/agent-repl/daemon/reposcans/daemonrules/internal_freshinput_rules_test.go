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

// internalFreshinputDir is the package these rules hold, under the daemon module's root.
const internalFreshinputDir = "internal/freshinput"

// handRolledSum matches a written/unwritten cache-miss sum spelled out inline:
// either miss read beside a `+`.
var handRolledSum = regexp.MustCompile(`GetWritten\(\)\s*\+|\+\s*[\w.()]*GetUnwritten\(\)|GetUnwritten\(\)\s*\+|\+\s*[\w.()]*GetWritten\(\)`)

func TestNoDaemonSiteHandRollsTheFreshInputSum(t *testing.T) {
	sourcescan.InPackage(t, internalFreshinputDir)
	// Arrange: every production source of the daemon's internal packages.
	root := ".."
	var offenders []string

	// Act.
	err := filepath.WalkDir(root, func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if d.IsDir() || !strings.HasSuffix(path, ".go") || strings.HasSuffix(path, "_test.go") {
			return nil
		}
		if filepath.Base(filepath.Dir(path)) == "freshinput" {
			return nil
		}
		src, err := os.ReadFile(path)
		if err != nil {
			return err
		}
		for i, line := range strings.Split(string(src), "\n") {
			if handRolledSum.MatchString(line) {
				offenders = append(offenders, fmt.Sprintf("%s:%d", path, i+1))
			}
		}
		return nil
	})

	// Assert: fresh input is summed in one place, freshinput.Of.
	if err != nil {
		t.Fatalf("walk: %v", err)
	}
	if len(offenders) > 0 {
		t.Fatalf("hand-rolled fresh-input sums (use freshinput.Of): %s", strings.Join(offenders, ", "))
	}
}
