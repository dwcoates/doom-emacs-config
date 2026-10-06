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

func TestShortBase(t *testing.T) {
	existing, err := os.MkdirTemp("/tmp", "arbase")
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { _ = os.RemoveAll(existing) })
	tests := []struct {
		name    string
		set     string
		want    string
		wantErr bool
	}{
		{name: "unset is /tmp", set: "", want: DefaultShortBase},
		{name: "a directory beneath /tmp is honored", set: existing, want: existing},
		{name: "a directory outside /tmp is refused", set: "/usr", wantErr: true},
		{name: "an absent directory is refused", set: existing + "/absent", wantErr: true},
		{name: "an unclean spelling is refused", set: existing + "/", wantErr: true},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			getenv := func(string) string { return tc.set }

			// Act
			got, err := ShortBase(getenv)

			// Assert
			if (err != nil) != tc.wantErr || got != tc.want {
				t.Fatalf("ShortBase = %q, %v; want %q (error %v)", got, err, tc.want, tc.wantErr)
			}
		})
	}
}
