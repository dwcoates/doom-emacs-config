// Package sourcescan reads the package under test's own production Go
// sources, for the tests that hold a package to a structural rule ("only
// endTx may roll back", "only connected() announces a link").
//
// Those tests each used to glob "*.go", skip "_test.go" and read what was
// left by hand, and a hand-rolled copy that forgot the skip would count a
// test's own mention of the pattern and pass or fail for the wrong reason.
package sourcescan

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

// File is one production source file: its name in the package directory and
// its contents.
type File struct {
	Name   string
	Source []byte
}

// Production returns every non-test .go file in the working directory, which
// `go test` sets to the package under test, sorted by name. A package with no
// production source fails the test: a structural rule over nothing proves
// nothing.
func Production(t testing.TB) []File {
	t.Helper()
	names, err := filepath.Glob("*.go")
	if err != nil {
		t.Fatalf("sourcescan: glob: %v", err)
	}
	sort.Strings(names)
	var files []File
	for _, name := range names {
		if strings.HasSuffix(name, "_test.go") {
			continue
		}
		source, err := os.ReadFile(name)
		if err != nil {
			t.Fatalf("sourcescan: read %s: %v", name, err)
		}
		files = append(files, File{Name: name, Source: source})
	}
	if len(files) == 0 {
		t.Fatalf("sourcescan: no production .go files in the package under test")
	}
	return files
}

// Count is how many times substr occurs across the package's production
// sources.
func Count(t testing.TB, substr string) int {
	t.Helper()
	n := 0
	for _, f := range Production(t) {
		n += strings.Count(string(f.Source), substr)
	}
	return n
}

// DaemonRoot answers the daemon module's root: the nearest directory at or
// above the package under test that holds go.mod.
func DaemonRoot(t testing.TB) string {
	t.Helper()
	dir, err := filepath.Abs(".")
	if err != nil {
		t.Fatalf("sourcescan: resolve the package directory: %v", err)
	}
	for {
		if _, err := os.Stat(filepath.Join(dir, "go.mod")); err == nil {
			return dir
		}
		parent := filepath.Dir(dir)
		if parent == dir {
			t.Fatalf("sourcescan: no go.mod above the package under test")
		}
		dir = parent
	}
}

// EachDaemonSource parses every production .go file under ROOT and hands each
// to VISIT with its file set: the daemon-wide counterpart of Production, for a
// rule that holds across packages ("every child is built in childproc").
//
// It skips tests, hidden directories, the integration and e2e harnesses
// (they start the daemon under test, so nothing in them runs inside a serving
// daemon), and every ROOT-relative directory in SKIP: typically the package
// that owns the rule, the one place it is allowed to break it.
func EachDaemonSource(t testing.TB, root string, skip []string, visit func(fset *token.FileSet, file *ast.File)) {
	t.Helper()
	fset := token.NewFileSet()
	err := filepath.WalkDir(root, func(path string, d fs.DirEntry, err error) error {
		if err != nil {
			return err
		}
		rel, err := filepath.Rel(root, path)
		if err != nil {
			return err
		}
		if d.IsDir() {
			if rel == "integration" || rel == "e2e" || slices.Contains(skip, rel) || (rel != "." && strings.HasPrefix(d.Name(), ".")) {
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
		t.Fatalf("sourcescan: walk the daemon sources: %v", err)
	}
}
