// Package sourcescan reads the package under test's own production Go
// sources, for the tests that hold a package to a structural rule ("only
// endTx may roll back", "only connected() announces a link").
//
// Those tests each used to glob "*.go", skip "_test.go" and read what was
// left by hand, and a hand-rolled copy that forgot the skip would count a
// test's own mention of the pattern and pass or fail for the wrong reason.
package sourcescan

import (
	"os"
	"path/filepath"
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
