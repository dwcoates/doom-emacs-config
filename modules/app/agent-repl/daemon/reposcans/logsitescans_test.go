package reposcans

import (
	"io/fs"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// logSiteScanFile is the name every Go module's log site scan test has.
const logSiteScanFile = "logsite_scan_test.go"

// handRolledLogSiteScan answers why SRC, a log site scan test, does not take
// the shared shape (scan.Require), or "" when it does.
func handRolledLogSiteScan(src string) string {
	switch {
	case !strings.Contains(src, "scan.Require("):
		return "does not call scan.Require"
	case strings.Contains(src, "scan.Scan(") || strings.Contains(src, "scan.Check("):
		return "scans or checks by hand beside scan.Require"
	case strings.Contains(src, "[]scan.Call{"):
		return "spells its call shapes by hand instead of scan.Calls"
	}
	return ""
}

func TestTheLogSiteScanGuardRecognizesAHandRolledScan(t *testing.T) {
	tests := []struct {
		name string
		src  string
		want string
	}{
		{name: "the shared shape", src: `var c = scan.Calls(3, 1, 2, "Info"); func T(t *testing.T) { scan.Require(t, spec) }`},
		{name: "no Require", src: `sites, err := scan.Scan(spec)`, want: "does not call scan.Require"},
		{name: "a check beside Require", src: `scan.Require(t, spec); for _, f := range scan.Check(sites) {}`, want: "scans or checks by hand beside scan.Require"},
		{name: "call shapes by hand", src: `var c = []scan.Call{{Selector: "Info"}}; scan.Require(t, spec)`, want: "spells its call shapes by hand instead of scan.Calls"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := handRolledLogSiteScan(tt.src)

			// Assert
			if got != tt.want {
				t.Fatalf("handRolledLogSiteScan = %q, want %q", got, tt.want)
			}
		})
	}
}

// TestEveryLogSiteScanTakesTheSharedShape fails a Go module's log site scan
// test that hand-rolls what scan.Require and scan.Calls do, so every
// runtime's scan fails the same way on the same faults.
func TestEveryLogSiteScanTakesTheSharedShape(t *testing.T) {
	// Arrange
	root := moduleRootDir(t)
	var found []string

	// Act
	err := filepath.WalkDir(root, func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if d.IsDir() {
			if d.Name() == "node_modules" || d.Name() == "testdata" {
				return filepath.SkipDir
			}
			return nil
		}
		if d.Name() != logSiteScanFile {
			return nil
		}
		src, err := os.ReadFile(path)
		if err != nil {
			return err
		}
		rel, _ := filepath.Rel(root, path)
		found = append(found, rel)
		if why := handRolledLogSiteScan(string(src)); why != "" {
			t.Errorf("%s %s", rel, why)
		}
		return nil
	})

	// Assert
	if err != nil {
		t.Fatalf("walk the module: %v", err)
	}
	if len(found) < 8 {
		t.Fatalf("found %d log site scan tests (%v), want one per Go runtime, at least 8", len(found), found)
	}
}
