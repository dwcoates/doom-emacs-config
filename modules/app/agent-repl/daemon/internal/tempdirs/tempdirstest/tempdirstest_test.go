package tempdirstest

import (
	"os"
	"path/filepath"
	"testing"

	"claude-repld/internal/tempdirs"
)

func TestMain(m *testing.M) { os.Exit(Main(m)) }

func TestGuardExemptsOnlyTheRunsOwnTemporaryRoot(t *testing.T) {
	tests := []struct {
		name    string
		dir     func(t *testing.T) string
		refused bool
	}{
		{name: "a directory from a test's TempDir", dir: func(t *testing.T) string { return filepath.Join(t.TempDir(), "repo") }, refused: false},
		{name: "a directory from os.MkdirTemp", dir: func(t *testing.T) string {
			dir, err := os.MkdirTemp("", "x")
			if err != nil {
				t.Fatal(err)
			}
			t.Cleanup(func() { _ = os.RemoveAll(dir) })
			return dir
		}, refused: false},
		{name: "another temporary folder", dir: func(*testing.T) string { return "/tmp/agent-repl-not-this-run" }, refused: true},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			g := Guard(t)

			// Act
			err := g.Check(tc.dir(t))

			// Assert
			if _, inside := tempdirs.AsInside(err); inside != tc.refused {
				t.Fatalf("Check = %v, want refused=%v", err, tc.refused)
			}
		})
	}
}
