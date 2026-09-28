package sourcescan

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// fakeTB records a Fatalf instead of ending the test, so the failure path is
// observable.
type fakeTB struct {
	testing.TB
	failed  bool
	message string
}

func (f *fakeTB) Helper() {}
func (f *fakeTB) Fatalf(format string, args ...any) {
	f.failed = true
	f.message = format
}

func packageDir(t *testing.T, files map[string]string) {
	t.Helper()
	dir := t.TempDir()
	for name, body := range files {
		if err := os.WriteFile(filepath.Join(dir, name), []byte(body), 0o600); err != nil {
			t.Fatalf("WriteFile: %v", err)
		}
	}
	t.Chdir(dir)
}

func TestProductionSkipsTestFilesAndSortsByName(t *testing.T) {
	// Arrange
	packageDir(t, map[string]string{"b.go": "package p // b", "a.go": "package p // a", "a_test.go": "package p // t", "notes.txt": "x"})

	// Act
	files := Production(t)

	// Assert
	if len(files) != 2 || files[0].Name != "a.go" || files[1].Name != "b.go" || string(files[0].Source) != "package p // a" {
		t.Fatalf("Production = %+v, want a.go then b.go with their sources", files)
	}
}

func TestProductionOfAPackageWithNoProductionSourceFails(t *testing.T) {
	// Arrange
	packageDir(t, map[string]string{"only_test.go": "package p"})
	fake := &fakeTB{TB: t}

	// Act
	Production(fake)

	// Assert
	if !fake.failed || !strings.Contains(fake.message, "no production .go files") {
		t.Fatalf("failed=%v message=%q, want the empty package refused", fake.failed, fake.message)
	}
}

func TestCountSumsAcrossProductionFilesAndIgnoresTests(t *testing.T) {
	// Arrange
	packageDir(t, map[string]string{"a.go": "x.Rollback() x.Rollback()", "b.go": "x.Rollback()", "a_test.go": "x.Rollback()"})

	// Act
	n := Count(t, ".Rollback()")

	// Assert
	if n != 3 {
		t.Fatalf("Count = %d, want 3 from production files only", n)
	}
}

// Every daemon test that reads its package's production sources goes through
// this package, so no hand-rolled copy can drift from the skip-the-tests rule.
func TestNoDaemonTestHandRollsAProductionSourceScan(t *testing.T) {
	// Arrange
	root := filepath.Join("..", "..")
	var offenders []string

	// Act
	err := filepath.WalkDir(root, func(path string, d os.DirEntry, err error) error {
		if err != nil {
			return err
		}
		if d.IsDir() || !strings.HasSuffix(path, "_test.go") || strings.Contains(path, string(filepath.Separator)+"sourcescan"+string(filepath.Separator)) {
			return nil
		}
		body, err := os.ReadFile(path)
		if err != nil {
			return err
		}
		if strings.Contains(string(body), `filepath.Glob("*.go")`) {
			offenders = append(offenders, path)
		}
		return nil
	})

	// Assert
	if err != nil {
		t.Fatalf("WalkDir: %v", err)
	}
	if len(offenders) != 0 {
		t.Fatalf("these tests glob their own sources instead of using sourcescan: %v", offenders)
	}
}
