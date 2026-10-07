package atomicfile

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// entries answers the names in dir.
func entries(t *testing.T, dir string) []string {
	t.Helper()
	list, err := os.ReadDir(dir)
	if err != nil {
		t.Fatalf("read %s: %v", dir, err)
	}
	names := make([]string, 0, len(list))
	for _, e := range list {
		names = append(names, e.Name())
	}
	return names
}

func TestReplaceWritesTheBody(t *testing.T) {
	// Arrange.
	path := filepath.Join(t.TempDir(), "f")
	if err := os.WriteFile(path, []byte("old"), 0o644); err != nil {
		t.Fatalf("write: %v", err)
	}

	// Act.
	err := Replace(path, []byte("new"), Options{})

	// Assert.
	got, _ := os.ReadFile(path)
	if err != nil || string(got) != "new" {
		t.Fatalf("Replace = %v, content %q, want nil and \"new\"", err, got)
	}
}

func TestReplaceCreatesAnAbsentFile(t *testing.T) {
	// Arrange.
	path := filepath.Join(t.TempDir(), "f")

	// Act.
	err := Replace(path, []byte("x"), Options{})

	// Assert.
	if got, _ := os.ReadFile(path); err != nil || string(got) != "x" {
		t.Fatalf("Replace = %v, content %q, want the file created", err, got)
	}
}

func TestReplaceLeavesNoTemporaryBehind(t *testing.T) {
	// Arrange.
	dir := t.TempDir()

	// Act.
	if err := Replace(filepath.Join(dir, "f"), []byte("x"), Options{Pattern: "f-*.tmp"}); err != nil {
		t.Fatalf("Replace: %v", err)
	}

	// Assert.
	if names := entries(t, dir); len(names) != 1 || names[0] != "f" {
		t.Fatalf("the directory holds %v, want only f", names)
	}
}

func TestReplaceSetsTheMode(t *testing.T) {
	// Arrange.
	path := filepath.Join(t.TempDir(), "f")

	// Act.
	if err := Replace(path, []byte("x"), Options{Mode: 0o640, Sync: true}); err != nil {
		t.Fatalf("Replace: %v", err)
	}

	// Assert.
	if info, _ := os.Stat(path); info.Mode().Perm() != 0o640 {
		t.Fatalf("mode = %v, want 0640", info.Mode().Perm())
	}
}

func TestReplaceWithNoModeKeepsTheTemporarysOwnerOnlyMode(t *testing.T) {
	// Arrange.
	path := filepath.Join(t.TempDir(), "f")

	// Act.
	if err := Replace(path, []byte("x"), Options{}); err != nil {
		t.Fatalf("Replace: %v", err)
	}

	// Assert.
	if info, _ := os.Stat(path); info.Mode().Perm() != 0o600 {
		t.Fatalf("mode = %v, want 0600", info.Mode().Perm())
	}
}

func TestReplaceFailsInAnAbsentDirectory(t *testing.T) {
	// Act.
	err := Replace(filepath.Join(t.TempDir(), "absent", "f"), []byte("x"), Options{})

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "create a temporary") {
		t.Fatalf("Replace = %v, want the temporary's failure", err)
	}
}

func TestReplaceRemovesTheTemporaryWhenTheRenameFails(t *testing.T) {
	// Arrange: a non-empty directory at path refuses the rename.
	dir := t.TempDir()
	path := filepath.Join(dir, "f")
	if err := os.MkdirAll(filepath.Join(path, "child"), 0o755); err != nil {
		t.Fatalf("mkdir: %v", err)
	}

	// Act.
	err := Replace(path, []byte("x"), Options{})

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "rename") {
		t.Fatalf("Replace = %v, want the rename's failure", err)
	}
	if names := entries(t, dir); len(names) != 1 || names[0] != "f" {
		t.Fatalf("the directory holds %v, want the temporary removed", names)
	}
}

// TestEverySiteReplacesThroughThisPackage holds the daemon's whole-file
// replaces to Replace: a site that hand-rolls its own temporary fails here.
func TestEverySiteReplacesThroughThisPackage(t *testing.T) {
	// Arrange.
	sites := []string{
		"../daemonaddr/claim.go",
		"../rollout/carry.go",
		"../rollout/manifest.go",
		"../rollout/spawn.go",
		"../commandfile/write.go",
		"../classifierupdate/update.go",
	}
	for _, site := range sites {
		t.Run(site, func(t *testing.T) {
			// Act.
			src, err := os.ReadFile(site)
			if err != nil {
				t.Fatalf("read %s: %v", site, err)
			}

			// Assert.
			if strings.Contains(string(src), "os.CreateTemp(") || !strings.Contains(string(src), "atomicfile.Replace(") {
				t.Fatalf("%s replaces a file without atomicfile.Replace", site)
			}
		})
	}
}
