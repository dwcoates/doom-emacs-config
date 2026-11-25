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

// internalCtxendDir is the package these rules hold, under the daemon module's root.
const internalCtxendDir = "internal/ctxend"

// handRolled is the classification Is replaces, spelled out at a call site.
var handRolled = regexp.MustCompile(`errors\.Is\((\w+(?:\.\w+)?), context\.Canceled\)\s*\|\|\s*errors\.Is\((\w+(?:\.\w+)?), context\.DeadlineExceeded\)`)

// TestNoSiteHandRollsTheClassification pins that every production site asks
// Is, so no site draws the line between a context ending and a failure
// somewhere else.
func TestNoSiteHandRollsTheClassification(t *testing.T) {
	sourcescan.InPackage(t, internalCtxendDir)
	// Arrange.
	var offenders []string
	for _, dir := range []string{"..", "../../cmd"} {
		err := filepath.WalkDir(dir, func(path string, d fs.DirEntry, err error) error {
			if err != nil || d.IsDir() || !strings.HasSuffix(path, ".go") || strings.HasSuffix(path, "_test.go") {
				return err
			}
			if filepath.Base(filepath.Dir(path)) == "ctxend" {
				return nil
			}
			src, err := os.ReadFile(path)
			if err != nil {
				return err
			}
			// Act.
			if handRolled.Match(src) {
				offenders = append(offenders, path)
			}
			return nil
		})
		if err != nil {
			t.Fatalf("walking %s: %v", dir, err)
		}
	}

	// Assert.
	if len(offenders) > 0 {
		t.Fatalf("these sites classify a context ending by hand instead of through ctxend.Is: %v", offenders)
	}
}

// TestNoSiteHandRollsTheLogSplit fails a site that spells out ctxend.Log by
// hand: a one-line cut record and a one-line failed record on the two sides
// of ctxend.Is, each followed by the same return.
func TestNoSiteHandRollsTheLogSplit(t *testing.T) {
	sourcescan.InPackage(t, internalCtxendDir)
	// Arrange.
	record := regexp.MustCompile(`^\s*\S+\.(Info|Debug|Error)\(.*\)$`)
	var offenders []string
	for _, dir := range []string{"..", "../../cmd"} {
		err := filepath.WalkDir(dir, func(path string, d fs.DirEntry, err error) error {
			if err != nil || d.IsDir() || !strings.HasSuffix(path, ".go") || strings.HasSuffix(path, "_test.go") {
				return err
			}
			src, err := os.ReadFile(path)
			if err != nil {
				return err
			}
			// Act.
			lines := strings.Split(string(src), "\n")
			for i := 0; i+5 < len(lines); i++ {
				if strings.TrimSpace(lines[i]) != "if ctxend.Is(err) {" {
					continue
				}
				cut, cutReturn, closing, failed, failedReturn := lines[i+1], strings.TrimSpace(lines[i+2]), strings.TrimSpace(lines[i+3]), lines[i+4], strings.TrimSpace(lines[i+5])
				if record.MatchString(cut) && !strings.Contains(cut, ".Error(") && strings.HasPrefix(cutReturn, "return") &&
					closing == "}" && strings.Contains(failed, ".Error(") && record.MatchString(failed) && failedReturn == cutReturn {
					offenders = append(offenders, fmt.Sprintf("%s:%d", path, i+1))
				}
			}
			return nil
		})
		if err != nil {
			t.Fatalf("walking %s: %v", dir, err)
		}
	}

	// Assert.
	if len(offenders) > 0 {
		t.Fatalf("these sites spell out ctxend.Log by hand: %v", offenders)
	}
}
