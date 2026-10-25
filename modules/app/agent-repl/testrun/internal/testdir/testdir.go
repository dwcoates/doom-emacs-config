// Package testdir locates a test's own files and the trees around them.
//
// A TEST FINDS ITS FILES FROM ITS WORKING DIRECTORY. `go test`, and the test
// runner running a prebuilt test binary, both start a test in its package's
// directory, so the working directory is the package directory by
// construction. runtime.Caller's compiled-in source path is never used: every
// Go build runs with -trimpath (internal/suites), which removes it.
package testdir

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// marker is the agent-repl module root, relative to its repository root.
const marker = "modules/app/agent-repl"

// Here is the running test's package directory.
func Here(t testing.TB) string {
	t.Helper()
	dir, err := os.Getwd()
	if err != nil {
		t.Fatalf("testdir: the working directory, which is the test's package directory: %v", err)
	}
	return dir
}

// ModuleRoot is the agent-repl module root (the directory ending in
// modules/app/agent-repl) the running test's package sits in.
func ModuleRoot(t testing.TB) string {
	t.Helper()
	root, err := moduleRootFrom(Here(t))
	if err != nil {
		t.Fatal(err)
	}
	return root
}

// RepoRoot is the repository root holding ModuleRoot.
func RepoRoot(t testing.TB) string {
	t.Helper()
	return repoRootOf(ModuleRoot(t))
}

// moduleRootFrom is the nearest ancestor of dir (dir included) that ends in
// the marker.
func moduleRootFrom(dir string) (string, error) {
	for at := filepath.Clean(dir); ; at = filepath.Dir(at) {
		if strings.HasSuffix(filepath.ToSlash(at), "/"+marker) {
			return at, nil
		}
		if filepath.Dir(at) == at {
			return "", fmt.Errorf("testdir: no ancestor of %q ends in %q", dir, marker)
		}
	}
}

// repoRootOf is root with the marker's segments removed.
func repoRootOf(root string) string {
	for range strings.Split(marker, "/") {
		root = filepath.Dir(root)
	}
	return root
}
