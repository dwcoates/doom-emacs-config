package main

import (
	"context"
	"errors"
	"io/fs"
	"os"
	"path/filepath"
	"testing"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"agent-repld/internal/shimclient"
)

// noSessionEnv is a command environment that names no session workspace.
func noSessionEnv(string) string { return "" }

// atCaller fills DEPS' caller seams for a command run in /wt/caller/src, the
// own worktree of the one workspace "caller": the commands' tests that need
// a caller and nothing else of it share this one shape.
func atCaller(deps cliDeps) cliDeps {
	deps.locate = locateOver(cliRow("ws-caller", "/wt/caller", "caller"))
	deps.getwd = func() (string, error) { return "/wt/caller/src", nil }
	deps.getenv = noSessionEnv
	deps.isWorktreeRoot = worktreeRoots("/wt/caller")
	deps.mintKey = func(prefix string) (string, error) { return prefix + "-1", nil }
	return deps
}

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

func TestAtCallerIsTheCallersOwnWorktree(t *testing.T) {
	// Arrange
	deps := atCaller(cliDeps{})

	// Act
	cwd, err := deps.getwd()
	if err != nil {
		t.Fatal(err)
	}
	root, rootErr := worktreeRoot(cwd, deps.isWorktreeRoot)
	refs, locateErr := deps.locate(context.Background(), nil, &agentreplv1.LocateWorkspaceRequest{By: &agentreplv1.LocateWorkspaceRequest_At{At: &agentreplv1.LocateWorkspaceAt{Dir: root}}})

	// Assert
	if rootErr != nil || locateErr != nil || root != "/wt/caller" || len(refs) != 1 || refs[0].GetId() != "ws-caller" ||
		deps.getenv(shimclient.EnvWorkspaceDir) != "" {
		t.Fatalf("root %q (%v), located %v (%v); want ws-caller's own worktree /wt/caller with no session", root, rootErr, refs, locateErr)
	}
}
