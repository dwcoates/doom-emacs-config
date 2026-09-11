package dlog

import (
	"crypto/md5"
	"encoding/hex"
	"os"
	"path/filepath"
	"testing"
)

func TestWorkspaceDirHashIsTheLockFileDerivation(t *testing.T) {
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
	want := hex.EncodeToString(sum[:])[:WorkspaceDirHashLength]

	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange, Act.
			got, err := WorkspaceDirHash(tc.dir)

			// Assert.
			if err != nil {
				t.Fatalf("WorkspaceDirHash(%q) error = %v", tc.dir, err)
			}
			if got != want {
				t.Fatalf("WorkspaceDirHash(%q) = %q, want %q — a record must grep against the lock file name", tc.dir, got, want)
			}
		})
	}
}

func TestWorkspaceDirHashAbsolutizesARelativeDir(t *testing.T) {
	// Arrange.
	cwd, err := os.Getwd()
	if err != nil {
		t.Fatalf("getwd: %v", err)
	}
	absHash, err := WorkspaceDirHash(filepath.Join(cwd, "sub"))
	if err != nil {
		t.Fatalf("WorkspaceDirHash(abs) error = %v", err)
	}

	// Act.
	relHash, err := WorkspaceDirHash("sub")

	// Assert.
	if err != nil {
		t.Fatalf("WorkspaceDirHash(rel) error = %v", err)
	}
	if relHash != absHash {
		t.Fatalf("relative hash %q != absolute hash %q", relHash, absHash)
	}
}

func TestWorkspaceDirHashIsEightCharacters(t *testing.T) {
	// Arrange, Act.
	got, err := WorkspaceDirHash("/tmp/whatever")

	// Assert.
	if err != nil {
		t.Fatalf("WorkspaceDirHash error = %v", err)
	}
	if len(got) != WorkspaceDirHashLength {
		t.Fatalf("len(%q) = %d, want %d", got, len(got), WorkspaceDirHashLength)
	}
}
