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
// the daemon opens its surface in exactly one place through OpenRunning, the
// store and the sidecar each through OpenService, and none assembles a
// registry or calls Open or OpenSelf itself, so no runtime can serve a
// surface without its build info or record its decisions its own way.
func TestEveryRuntimeOpensItsMetricsThroughTheSharedHelpers(t *testing.T) {
	// Arrange
	root := moduleRootDir(t)
	runtimes := map[string]struct {
		dir    string
		helper string
	}{
		"daemon":  {filepath.Join(root, "daemon"), "OpenRunning"},
		"store":   {filepath.Join(root, "agent-shim", "shim-store"), "OpenService"},
		"sidecar": {filepath.Join(root, "agent-shim", "claude", "shim-sidecar"), "OpenService"},
	}
	direct := regexp.MustCompile(`\bmetrics\.(Open|OpenSelf|NewRegistry)\(`)

	for name, rt := range runtimes {
		shared := regexp.MustCompile(`\bmetrics\.` + rt.helper + `\(`)
		dir := rt.dir
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
			t.Errorf("%s opens its metrics surface through metrics.%s in %v, want exactly one site", name, rt.helper, sharedUses)
		}
		if len(directUses) != 0 {
			t.Errorf("%s calls metrics.Open, OpenSelf or NewRegistry directly in %v", name, directUses)
		}
	}
}

// TestTheDaemonBuildsEveryHistogramSeriesInOnePlace pins the daemon's one
// conversion of its own running histograms into served series
// (runtimemetrics histogram.metric): a collector that built its constant
// histogram by hand could serve its buckets differently from every other.
func TestTheDaemonBuildsEveryHistogramSeriesInOnePlace(t *testing.T) {
	// Arrange
	dir := filepath.Join(moduleRootDir(t), "daemon", "internal")
	direct := regexp.MustCompile(`\bprometheus\.MustNewConstHistogram\(`)

	// Act
	var uses []string
	err := filepath.WalkDir(dir, func(path string, d os.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if d.IsDir() || !strings.HasSuffix(path, ".go") || strings.HasSuffix(path, "_test.go") {
			return nil
		}
		raw, err := os.ReadFile(path)
		if err != nil {
			return err
		}
		for range direct.FindAll(raw, -1) {
			uses = append(uses, path)
		}
		return nil
	})

	// Assert
	if err != nil {
		t.Fatalf("walk %s: %v", dir, err)
	}
	want := filepath.Join(dir, "runtimemetrics", "latency.go")
	if len(uses) != 1 || uses[0] != want {
		t.Fatalf("prometheus.MustNewConstHistogram is called in %v, want only in %s (histogram.metric)", uses, want)
	}
}
