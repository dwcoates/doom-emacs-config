package kit

import (
	"os"
	"path/filepath"
	"testing"
)

func TestResolvedPathResolvesASymlinkedDirectory(t *testing.T) {
	t.Parallel()
	// Arrange: a directory reached through a symlink, exactly the shape a
	// macOS temp root has (/tmp -> /private/tmp).
	real := filepath.Join(t.TempDir(), "real")
	if err := os.Mkdir(real, 0o755); err != nil {
		t.Fatalf("make the real dir: %v", err)
	}
	link := filepath.Join(t.TempDir(), "link")
	if err := os.Symlink(real, link); err != nil {
		t.Fatalf("symlink: %v", err)
	}

	// Act.
	got, err := ResolvedPath(link)

	// Assert: the resolved target, not the link.
	if err != nil {
		t.Fatalf("ResolvedPath(%s) = error %v", link, err)
	}
	want, err := filepath.EvalSymlinks(real)
	if err != nil {
		t.Fatalf("resolve the real dir: %v", err)
	}
	if got != want {
		t.Fatalf("ResolvedPath(%s) = %q, want %q", link, got, want)
	}
}

func TestResolvedPathLeavesAnUnsymlinkedDirectoryUnchanged(t *testing.T) {
	t.Parallel()
	// Arrange: an already-resolved directory.
	dir, err := filepath.EvalSymlinks(t.TempDir())
	if err != nil {
		t.Fatalf("resolve the temp dir: %v", err)
	}

	// Act.
	got, err := ResolvedPath(dir)

	// Assert.
	if err != nil {
		t.Fatalf("ResolvedPath(%s) = error %v", dir, err)
	}
	if got != dir {
		t.Fatalf("ResolvedPath(%s) = %q, want it unchanged", dir, got)
	}
}

func TestResolvedPathFailsLoudlyOnAMissingDirectory(t *testing.T) {
	t.Parallel()
	// Arrange: a path nothing created.
	missing := filepath.Join(t.TempDir(), "absent")

	// Act.
	_, err := ResolvedPath(missing)

	// Assert: an error, never the path itself.
	if err == nil {
		t.Fatalf("ResolvedPath(%s) = nil error, want a loud failure", missing)
	}
}
