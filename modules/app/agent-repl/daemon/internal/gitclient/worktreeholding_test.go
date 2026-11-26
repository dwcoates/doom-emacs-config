package gitclient

import (
	"context"
	"errors"
	"testing"
)

// fakeWorktreeLister answers WorktreeHolding's two git reads: the main
// worktree (or mainErr), and the worktrees under it (or listErr).
type fakeWorktreeLister struct {
	main      string
	mainErr   error
	worktrees []Worktree
	listErr   error
	// listedIn is the directory the listing was asked of.
	listedIn string
}

func (f *fakeWorktreeLister) MainWorktree(context.Context, string) (string, error) {
	return f.main, f.mainErr
}

func (f *fakeWorktreeLister) ListWorktrees(_ context.Context, repoDir string) ([]Worktree, error) {
	f.listedIn = repoDir
	return f.worktrees, f.listErr
}

func TestWorktreeHoldingFindsTheOneWorktreeWithTheBranch(t *testing.T) {
	tests := []struct {
		name      string
		worktrees []Worktree
		wantDir   string
		wantHeld  bool
	}{
		{name: "a worktree has the branch", worktrees: []Worktree{{Dir: "/w/other", Branch: "other"}, {Dir: "/w/fix", Branch: "fix"}}, wantDir: "/w/fix", wantHeld: true},
		{name: "none has it", worktrees: []Worktree{{Dir: "/w/other", Branch: "other"}}},
		{name: "a bare entry never holds it", worktrees: []Worktree{{Dir: "/w/repo.git", Branch: "fix", Bare: true}}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			git := &fakeWorktreeLister{main: "/w/main", worktrees: tt.worktrees}

			// Act
			dir, main, held, err := WorktreeHolding(context.Background(), git, "/w/in", "fix")

			// Assert
			if err != nil || dir != tt.wantDir || held != tt.wantHeld || main != "/w/main" {
				t.Fatalf("WorktreeHolding = %q, %q, %v, %v; want %q, the main worktree, %v", dir, main, held, err, tt.wantDir, tt.wantHeld)
			}
			if git.listedIn != "/w/main" {
				t.Fatalf("listed the worktrees of %q, want the main worktree's", git.listedIn)
			}
		})
	}
}

func TestWorktreeHoldingFailsWhenAGitReadFails(t *testing.T) {
	tests := []struct {
		name string
		git  *fakeWorktreeLister
	}{
		{name: "the main worktree cannot be read", git: &fakeWorktreeLister{mainErr: errors.New("not a repository")}},
		{name: "the worktrees cannot be listed", git: &fakeWorktreeLister{main: "/w/main", listErr: errors.New("locked")}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			_, _, held, err := WorktreeHolding(context.Background(), tt.git, "/w/in", "fix")

			// Assert
			if err == nil || held {
				t.Fatalf("WorktreeHolding = %v, %v; want the failure", held, err)
			}
		})
	}
}
