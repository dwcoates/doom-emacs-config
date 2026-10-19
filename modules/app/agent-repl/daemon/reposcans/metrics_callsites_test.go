package reposcans

import (
	"encoding/json"
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

// TestTheStoreFileNamesTheStoreTheServiceRuns pins metrics/store.json, the
// address every runtime pushes its final values to, to the store the
// launchd plist runs and the job its scrape config names: a push to another
// port is lost, and a push under another job writes series no scrape writes.
func TestTheStoreFileNamesTheStoreTheServiceRuns(t *testing.T) {
	// Arrange
	root := moduleRootDir(t)
	read := func(rel string) string {
		raw, err := os.ReadFile(filepath.Join(root, rel))
		if err != nil {
			t.Fatalf("read %s: %v", rel, err)
		}
		return string(raw)
	}
	var store struct {
		ImportURL string `json:"import_url"`
		Job       string `json:"job"`
	}
	if err := json.Unmarshal([]byte(read("metrics/store.json")), &store); err != nil {
		t.Fatalf("decode metrics/store.json: %v", err)
	}
	listen := regexp.MustCompile(`-httpListenAddr=([0-9.]+:[0-9]+)`).FindStringSubmatch(read("launchd/com.agentrepl.victoriametrics.plist"))
	job := regexp.MustCompile(`(?m)^\s*-\s*job_name:\s*(\S+)`).FindStringSubmatch(read("metrics/scrape.yml"))

	// Act
	wantURL := ""
	if listen != nil {
		wantURL = "http://" + listen[1] + "/api/v1/import/prometheus"
	}

	// Assert
	if listen == nil || store.ImportURL != wantURL {
		t.Fatalf("store.json import_url = %q, want %q (the plist's -httpListenAddr)", store.ImportURL, wantURL)
	}
	if job == nil || store.Job != job[1] {
		t.Fatalf("store.json job = %q, want scrape.yml's job_name %v", store.Job, job)
	}
}
