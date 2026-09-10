package logging

import (
	"crypto/md5"
	"encoding/hex"
	"path/filepath"
	"testing"
)

func TestWorkspaceIDUsesTheCanonicalAbsoluteCleanPath(t *testing.T) {
	// Arrange.
	dir := filepath.Join(t.TempDir(), "project", "..", "project")
	abs, err := filepath.Abs(dir)
	if err != nil {
		t.Fatalf("absolute fixture path: %v", err)
	}
	sum := md5.Sum([]byte(filepath.Clean(abs)))
	want := hex.EncodeToString(sum[:])[:WorkspaceIDLength]

	// Act.
	got, err := WorkspaceID(dir)

	// Assert.
	if err != nil {
		t.Fatalf("WorkspaceID = %v", err)
	}
	if got != want {
		t.Fatalf("WorkspaceID = %q, want %q", got, want)
	}
}
