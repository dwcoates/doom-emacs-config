package metrics

import (
	"os"
	"path/filepath"
	"regexp"
	"strings"
	"testing"
)

// TestEveryRuntimeOpensThroughTheRunningBuild pins the shared shape: the
// daemon, the store and the sidecar open their surface through OpenRunning or
// OpenSelf, never by assembling a registry and calling Open themselves, so no
// runtime can serve a surface without its build info or with a hand-rolled
// error path.
func TestEveryRuntimeOpensThroughTheRunningBuild(t *testing.T) {
	// Arrange
	moduleRoot := filepath.Join("..", "..", "..", "..")
	runtimes := map[string]string{
		"daemon":  filepath.Join(moduleRoot, "daemon"),
		"store":   filepath.Join(moduleRoot, "agent-shim", "shim-store"),
		"sidecar": filepath.Join(moduleRoot, "agent-shim", "claude", "shim-sidecar"),
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
			if d.IsDir() && (d.Name() == "node_modules" || d.Name() == "testdata") {
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
