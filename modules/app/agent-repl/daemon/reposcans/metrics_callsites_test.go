package reposcans

import (
	"os"
	"path/filepath"
	"regexp"
	"strings"
	"testing"
)

// TestEveryRuntimeOpensItsMetricsThroughTheSharedHelpers pins the shape the
// shared metrics package (agentrepl/logging/metrics) gives every Go runtime:
// the daemon, the store and the sidecar each open their surface in exactly
// one place through OpenRunning or OpenSelf, and none assembles a registry
// and calls Open itself, so no runtime can serve a surface without its build
// info or with a hand-rolled error path.
func TestEveryRuntimeOpensItsMetricsThroughTheSharedHelpers(t *testing.T) {
	// Arrange
	root := moduleRootDir(t)
	runtimes := map[string]string{
		"daemon":  filepath.Join(root, "daemon"),
		"store":   filepath.Join(root, "agent-shim", "shim-store"),
		"sidecar": filepath.Join(root, "agent-shim", "claude", "shim-sidecar"),
	}
	shared := regexp.MustCompile(`\bmetrics\.(OpenRunning|OpenSelf)\(`)
	direct := regexp.MustCompile(`\bmetrics\.(Open|NewRegistry)\(`)

	for name, dir := range runtimes {
		// Act
		var sharedUses, directUses []string
		err := filepath.WalkDir(dir, func(path string, d os.DirEntry, err error) error {
			if err != nil {
				return err
			}
			if d.IsDir() && (d.Name() == "node_modules" || d.Name() == "testdata" || d.Name() == "reposcans") {
				return filepath.SkipDir
			}
			if d.IsDir() || !strings.HasSuffix(path, ".go") || strings.HasSuffix(path, "_test.go") {
				return nil
			}
			raw, err := os.ReadFile(path)
			if err != nil {
				return err
			}
			if shared.Match(raw) {
				sharedUses = append(sharedUses, path)
			}
			if direct.Match(raw) {
				directUses = append(directUses, path)
			}
			return nil
		})

		// Assert
		if err != nil {
			t.Fatalf("walk %s: %v", dir, err)
		}
		if len(sharedUses) != 1 {
			t.Errorf("%s opens its metrics surface through the shared helpers in %v, want exactly one site", name, sharedUses)
		}
		if len(directUses) != 0 {
			t.Errorf("%s calls metrics.Open or metrics.NewRegistry directly in %v", name, directUses)
		}
	}
}
