package sourcescan

import (
	"go/ast"
	"go/token"
	"os"
	"path/filepath"
	"slices"
	"sort"
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

func TestSitesNamesEachOccurrencesFileInOrderAndIgnoresTests(t *testing.T) {
	// Arrange
	packageDir(t, map[string]string{"b.go": "x.Rollback()", "a.go": "x.Rollback() x.Rollback()", "a_test.go": "x.Rollback()"})

	// Act
	sites := Sites(t, ".Rollback()")

	// Assert
	if !slices.Equal(sites, []string{"a.go", "a.go", "b.go"}) {
		t.Fatalf("Sites = %v, want a.go twice then b.go, from production files only", sites)
	}
}

func TestSitesOfAnAbsentPatternIsEmpty(t *testing.T) {
	// Arrange
	packageDir(t, map[string]string{"a.go": "x.Commit()"})

	// Act
	sites := Sites(t, ".Rollback()")

	// Assert
	if len(sites) != 0 {
		t.Fatalf("Sites = %v, want none", sites)
	}
}

// Every one-home test lists its sites through Sites, so no hand-rolled copy
// can drift from it.
func TestNoDaemonTestHandRollsASitesList(t *testing.T) {
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
		if strings.Contains(string(body), "sites = append(sites, file.Name)") {
			offenders = append(offenders, path)
		}
		return nil
	})

	// Assert
	if err != nil {
		t.Fatalf("WalkDir: %v", err)
	}
	if len(offenders) != 0 {
		t.Fatalf("these tests list their sites by hand instead of with sourcescan.Sites: %v", offenders)
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

func TestEachDaemonSourceVisitsOnlyServingProductionSources(t *testing.T) {
	cases := []struct {
		name string
		skip []string
		want []string
	}{
		{name: "the harnesses, tests, hidden directories and other files are skipped", want: []string{"cmd/main.go", "internal/owner/z.go", "internal/pkg/b.go"}},
		{name: "a named directory is skipped too", skip: []string{"internal/owner"}, want: []string{"cmd/main.go", "internal/pkg/b.go"}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			root := t.TempDir()
			for _, rel := range []string{
				"cmd/main.go", "internal/pkg/b.go", "internal/owner/z.go",
				"internal/pkg/b_test.go", "integration/x.go", "e2e/y.go", ".hidden/w.go", "internal/pkg/notes.md",
			} {
				path := filepath.Join(root, rel)
				if err := os.MkdirAll(filepath.Dir(path), 0o755); err != nil {
					t.Fatalf("mkdir: %v", err)
				}
				if err := os.WriteFile(path, []byte("package p\n"), 0o600); err != nil {
					t.Fatalf("write %s: %v", rel, err)
				}
			}
			var visited []string

			// Act
			EachDaemonSource(t, root, tc.skip, func(fset *token.FileSet, file *ast.File) {
				rel, _ := filepath.Rel(root, fset.Position(file.Pos()).Filename)
				visited = append(visited, rel)
			})

			// Assert
			sort.Strings(visited)
			if !slices.Equal(visited, tc.want) {
				t.Fatalf("visited %v, want %v", visited, tc.want)
			}
		})
	}
}

func TestDaemonRootIsTheModuleRoot(t *testing.T) {
	// Arrange: this package sits two levels below the daemon module.
	want, err := filepath.Abs("../..")
	if err != nil {
		t.Fatalf("Abs: %v", err)
	}

	// Act
	root := DaemonRoot(t)

	// Assert
	if root != want {
		t.Fatalf("DaemonRoot = %q, want %q", root, want)
	}
}
