package main

import (
	"errors"
	"io/fs"
	"os"
	"path/filepath"
	"testing"
)

// noSessionEnv is a command environment that names no session workspace.
func noSessionEnv(string) string { return "" }

// worktreeRoots answers that exactly ROOTS are git worktrees' roots.
func worktreeRoots(roots ...string) func(string) (bool, error) {
	return func(dir string) (bool, error) {
		for _, root := range roots {
			if dir == root {
				return true, nil
			}
		}
		return false, nil
	}
}

func TestWorktreeRoot(t *testing.T) {
	statFailed := errors.New("permission denied")
	tests := []struct {
		name    string
		dir     string
		isRoot  func(string) (bool, error)
		want    string
		wantErr error
	}{
		{name: "the root itself is its own worktree", dir: "/repo", isRoot: worktreeRoots("/repo"), want: "/repo"},
		{name: "a subdirectory is in its root's worktree", dir: "/repo/modules/app", isRoot: worktreeRoots("/repo"), want: "/repo"},
		{name: "a nested separate worktree is its own, not the outer one's", dir: "/repo/.claude/worktrees/agent-1/src",
			isRoot: worktreeRoots("/repo", "/repo/.claude/worktrees/agent-1"), want: "/repo/.claude/worktrees/agent-1"},
		{name: "a directory in no worktree has none", dir: "/elsewhere/deep", isRoot: worktreeRoots("/repo"), want: ""},
		{name: "an unreadable mark is surfaced", dir: "/repo/src", isRoot: func(string) (bool, error) { return false, statFailed }, wantErr: statFailed},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got, err := worktreeRoot(tt.dir, tt.isRoot)

			// Assert
			if got != tt.want || !errors.Is(err, tt.wantErr) {
				t.Fatalf("worktreeRoot = %q, %v; want %q, %v", got, err, tt.want, tt.wantErr)
			}
		})
	}
}

func TestIsWorktreeRoot(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(t *testing.T, dir string)
		want    bool
		wantErr error
	}{
		{name: "a main checkout's .git directory marks a root", arrange: func(t *testing.T, dir string) {
			if err := os.Mkdir(filepath.Join(dir, ".git"), 0o755); err != nil {
				t.Fatal(err)
			}
		}, want: true},
		{name: "a linked worktree's .git file marks a root", arrange: func(t *testing.T, dir string) {
			if err := os.WriteFile(filepath.Join(dir, ".git"), []byte("gitdir: /elsewhere\n"), 0o644); err != nil {
				t.Fatal(err)
			}
		}, want: true},
		{name: "no .git entry marks no root", arrange: func(*testing.T, string) {}, want: false},
		{name: "an unreadable directory is an error", arrange: func(t *testing.T, dir string) {
			if err := os.Chmod(dir, 0o000); err != nil {
				t.Fatal(err)
			}
			t.Cleanup(func() { _ = os.Chmod(dir, 0o755) })
		}, wantErr: fs.ErrPermission},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			dir := t.TempDir()
			tt.arrange(t, dir)

			// Act
			got, err := isWorktreeRoot(dir)

			// Assert
			if got != tt.want || !errors.Is(err, tt.wantErr) {
				t.Fatalf("isWorktreeRoot = %v, %v; want %v, %v", got, err, tt.want, tt.wantErr)
			}
		})
	}
}
