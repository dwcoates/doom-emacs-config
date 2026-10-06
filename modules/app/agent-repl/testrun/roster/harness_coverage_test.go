package roster

import (
	"io/fs"
	"path/filepath"
	"runtime"
	"slices"
	"strings"
	"testing"
)

// harnessScripts answers every test-named shell harness in the places the
// repository keeps them, spelled as Suite.Path spells it: module-relative,
// or "/"-prefixed and repository-relative outside the module.
func harnessScripts(t *testing.T, module, repo string) []string {
	t.Helper()
	var found []string
	for _, glob := range []string{"bin/test-*.sh", "scripts/test-*.sh"} {
		matches, err := filepath.Glob(filepath.Join(module, glob))
		if err != nil {
			t.Fatal(err)
		}
		for _, m := range matches {
			rel, _ := filepath.Rel(module, m)
			found = append(found, rel)
		}
	}
	for _, glob := range []string{".githooks/test-*.sh", "bin/test-*.sh"} {
		matches, err := filepath.Glob(filepath.Join(repo, glob))
		if err != nil {
			t.Fatal(err)
		}
		for _, m := range matches {
			rel, _ := filepath.Rel(repo, m)
			found = append(found, "/"+rel)
		}
	}
	err := filepath.WalkDir(filepath.Join(repo, ".claude"), func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if !d.IsDir() && strings.HasPrefix(d.Name(), "test") && strings.HasSuffix(d.Name(), ".sh") {
			rel, _ := filepath.Rel(repo, path)
			found = append(found, "/"+rel)
		}
		return nil
	})
	if err != nil {
		t.Fatal(err)
	}
	return found
}

func TestEveryHarnessScriptIsRosteredOrExcluded(t *testing.T) {
	// Arrange
	_, self, _, ok := runtime.Caller(0)
	if !ok {
		t.Fatal("runtime.Caller could not locate this file")
	}
	module := filepath.Clean(filepath.Join(filepath.Dir(self), "..", ".."))
	repo := filepath.Clean(filepath.Join(module, "..", "..", ".."))
	scripts := harnessScripts(t, module, repo)
	if len(scripts) == 0 {
		t.Fatal("found no harness scripts: the layout this test assumes moved")
	}
	rostered := map[string]bool{}
	for _, s := range Suites {
		rostered[s.Path] = true
	}

	// Act
	var forgotten []string
	for _, path := range scripts {
		if _, excluded := NotSuites[path]; !rostered[path] && !excluded {
			forgotten = append(forgotten, path)
		}
	}

	// Assert
	if len(forgotten) > 0 {
		slices.Sort(forgotten)
		t.Fatalf("these harnesses run in no suite; add each to roster.Suites, or to roster.NotSuites with why:\n%s", strings.Join(forgotten, "\n"))
	}
}

func TestEveryExclusionNamesAScriptThatExists(t *testing.T) {
	// Arrange
	_, self, _, ok := runtime.Caller(0)
	if !ok {
		t.Fatal("runtime.Caller could not locate this file")
	}
	module := filepath.Clean(filepath.Join(filepath.Dir(self), "..", ".."))
	repo := filepath.Clean(filepath.Join(module, "..", "..", ".."))
	scripts := harnessScripts(t, module, repo)

	// Act / Assert
	for path, why := range NotSuites {
		if why == "" {
			t.Errorf("%s is excluded without a reason", path)
		}
		if !slices.Contains(scripts, path) {
			t.Errorf("%s is excluded but is no harness script any more; drop the stale entry", path)
		}
	}
}
