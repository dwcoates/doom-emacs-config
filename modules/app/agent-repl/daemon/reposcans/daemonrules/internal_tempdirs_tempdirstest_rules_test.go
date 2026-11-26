package daemonrules

import (
	"io/fs"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"agent-repld/reposcans/internal/sourcescan"
)

// TestNoTestMakesItsOwnShortSocketDirectory pins the call sites to the one
// shape: a daemon test that needs a directory short enough for a unix socket
// takes it from tempdirstest.ShortDir, which makes it in the run's short base
// (the RAM disk of a scheduled run) and removes it when the test ends. A test
// that hand-rolls `os.MkdirTemp("/tmp", ...)` fails here.
func TestNoTestMakesItsOwnShortSocketDirectory(t *testing.T) {
	// Arrange: the places that may name /tmp themselves. tempdirstest is the one
	// home of the helper, boot_test's long state root is a path the socket limit
	// refuses on purpose, and the harness's prompt-link test needs /tmp itself:
	// another filesystem than a test run's RAM-disk root.
	sanctioned := map[string]bool{
		"internal/tempdirs/tempdirstest/shortbase.go":         true,
		"internal/tempdirs/tempdirstest/tempdirstest_test.go": true,
		"integration/boot_test.go":                            true,
		"integration/harness/fakes_test.go":                   true,
	}
	root := sourcescan.DaemonRoot(t)
	var offenders []string

	// Act
	err := filepath.WalkDir(root, func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if d.IsDir() && (strings.HasPrefix(d.Name(), ".") || d.Name() == "reposcans" || d.Name() == "node_modules") {
			return fs.SkipDir
		}
		if d.IsDir() || !strings.HasSuffix(path, ".go") {
			return nil
		}
		rel, err := filepath.Rel(root, path)
		if err != nil {
			return err
		}
		body, err := os.ReadFile(path)
		if err != nil {
			return err
		}
		isTest := strings.HasSuffix(path, "_test.go") || filepath.Base(filepath.Dir(path)) == "h2ctest"
		if isTest && !sanctioned[filepath.ToSlash(rel)] && strings.Contains(string(body), `os.MkdirTemp("/tmp"`) {
			offenders = append(offenders, filepath.ToSlash(rel))
		}
		return nil
	})

	// Assert
	if err != nil {
		t.Fatalf("walk the daemon: %v", err)
	}
	if len(offenders) > 0 {
		t.Fatalf("these test files make their own short socket directory instead of tempdirstest.ShortDir: %v", offenders)
	}
}
