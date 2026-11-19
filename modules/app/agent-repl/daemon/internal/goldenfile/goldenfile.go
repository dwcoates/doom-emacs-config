// Package goldenfile holds a test's expected output in a checked-in file: a
// test compares what it computes with the file, and rewrites the file instead
// when run with its update flag (`go test ./<pkg> -run <Test> -update`). Every
// daemon test that keeps a golden file checks it here, so each reports a stale
// file the same way.
package goldenfile

import (
	"bytes"
	"os"
	"testing"
)

// Check fails T when the file at PATH does not hold WANT. With UPDATE it
// first writes WANT there, so the check then passes.
func Check(t testing.TB, path string, want []byte, update bool) {
	t.Helper()
	if update {
		if err := os.WriteFile(path, want, 0o644); err != nil {
			t.Fatalf("write %s: %v", path, err)
		}
	}
	got, err := os.ReadFile(path)
	if err != nil {
		t.Fatalf("read %s: %v", path, err)
	}
	if !bytes.Equal(got, want) {
		t.Errorf("%s is stale: rerun with -update", path)
	}
}
