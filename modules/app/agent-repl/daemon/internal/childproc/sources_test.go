package childproc

import (
	"go/ast"
	"go/parser"
	"go/token"
	"io/fs"
	"os"
	"path/filepath"
	"slices"
	"sort"
	"strings"
	"testing"
)

// eachDaemonSource parses every production Go source of the daemon that can
// start a child of a serving daemon, and hands each to visit with its file
// set. It skips this package (the one place children are built), the
// integration and e2e harnesses (they start the daemon under test, not a
// child of one), hidden directories and tests.
func eachDaemonSource(t *testing.T, root string, visit func(fset *token.FileSet, file *ast.File)) {
	t.Helper()
	fset := token.NewFileSet()
	err := filepath.WalkDir(root, func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		rel, _ := filepath.Rel(root, path)
		if d.IsDir() {
			if rel == "integration" || rel == "e2e" || rel == "internal/childproc" || strings.HasPrefix(d.Name(), ".") {
				return filepath.SkipDir
			}
			return nil
		}
		if !strings.HasSuffix(path, ".go") || strings.HasSuffix(path, "_test.go") {
			return nil
		}
		file, err := parser.ParseFile(fset, path, nil, parser.SkipObjectResolution)
		if err != nil {
			return err
		}
		visit(fset, file)
		return nil
	})
	if err != nil {
		t.Fatalf("walk the daemon sources: %v", err)
	}
}

// daemonRoot is the daemon module's root, which the source scans walk.
func daemonRoot(t *testing.T) string {
	t.Helper()
	root, err := filepath.Abs("../..")
	if err != nil {
		t.Fatalf("resolve the daemon root: %v", err)
	}
	return root
}

func TestEachDaemonSourceVisitsOnlyServingProductionSources(t *testing.T) {
	// Arrange
	root := t.TempDir()
	files := []string{
		"cmd/main.go", "internal/pkg/b.go",
		"internal/pkg/b_test.go", "integration/x.go", "e2e/y.go",
		"internal/childproc/z.go", ".hidden/w.go", "internal/pkg/notes.md",
	}
	for _, rel := range files {
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
	eachDaemonSource(t, root, func(fset *token.FileSet, file *ast.File) {
		rel, _ := filepath.Rel(root, fset.Position(file.Pos()).Filename)
		visited = append(visited, rel)
	})

	// Assert
	sort.Strings(visited)
	if want := []string{"cmd/main.go", "internal/pkg/b.go"}; !slices.Equal(visited, want) {
		t.Fatalf("visited %v, want %v", visited, want)
	}
}

// TestTheSourceScansShareOneWalk holds this package's scans to
// eachDaemonSource: a scan with a walk of its own would drift in what it skips.
func TestTheSourceScansShareOneWalk(t *testing.T) {
	// Arrange
	matches, err := filepath.Glob("*_test.go")
	if err != nil {
		t.Fatalf("glob: %v", err)
	}
	var offenders []string

	// Act
	for _, name := range matches {
		if name == "sources_test.go" {
			continue
		}
		raw, err := os.ReadFile(name)
		if err != nil {
			t.Fatalf("read %s: %v", name, err)
		}
		if strings.Contains(string(raw), "filepath.WalkDir(") || strings.Contains(string(raw), "parser.ParseFile(") {
			offenders = append(offenders, name)
		}
	}

	// Assert
	if len(offenders) > 0 {
		t.Fatalf("these files walk the daemon sources themselves; use eachDaemonSource: %v", offenders)
	}
}
