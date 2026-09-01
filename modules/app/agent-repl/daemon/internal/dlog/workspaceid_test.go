package dlog

import (
	"crypto/md5"
	"encoding/hex"
	"os"
	"path/filepath"
	"testing"
)

func TestLogWorkspaceIDIsTheLockFileDerivation(t *testing.T) {
	tests := []struct {
		name string
		dir  string
	}{
		{name: "plain path", dir: "/Users/someone/workspace/repo"},
		{name: "trailing separator", dir: "/Users/someone/workspace/repo/"},
		{name: "dot segment", dir: "/Users/someone/workspace/./repo"},
		{name: "parent segment", dir: "/Users/someone/workspace/other/../repo"},
	}
	// The lock file name the shim uses: md5hex(filepath.Clean(absDir))[:8].
	sum := md5.Sum([]byte("/Users/someone/workspace/repo"))
	want := hex.EncodeToString(sum[:])[:LogWorkspaceIDLength]

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange, Act.
			got, err := LogWorkspaceID(tc.dir)

			// Assert.
			if err != nil {
				t.Fatalf("LogWorkspaceID(%q) error = %v", tc.dir, err)
			}
			if got != want {
				t.Fatalf("LogWorkspaceID(%q) = %q, want %q — a record must grep against the lock file name", tc.dir, got, want)
			}
		})
	}
}

func TestLogWorkspaceIDAbsolutizesARelativeDir(t *testing.T) {
	// Arrange.
	cwd, err := os.Getwd()
	if err != nil {
		t.Fatalf("getwd: %v", err)
	}
	absID, err := LogWorkspaceID(filepath.Join(cwd, "sub"))
	if err != nil {
		t.Fatalf("LogWorkspaceID(abs) error = %v", err)
	}

	// Act.
	relID, err := LogWorkspaceID("sub")

	// Assert.
	if err != nil {
		t.Fatalf("LogWorkspaceID(rel) error = %v", err)
	}
	if relID != absID {
		t.Fatalf("relative id %q != absolute id %q", relID, absID)
	}
}

func TestLogWorkspaceIDIsEightCharacters(t *testing.T) {
	// Arrange, Act.
	got, err := LogWorkspaceID("/tmp/whatever")

	// Assert.
	if err != nil {
		t.Fatalf("LogWorkspaceID error = %v", err)
	}
	if len(got) != LogWorkspaceIDLength {
		t.Fatalf("len(%q) = %d, want %d", got, len(got), LogWorkspaceIDLength)
	}
}
