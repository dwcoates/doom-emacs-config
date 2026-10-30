package workspace

import (
	"context"
	"errors"
	"testing"
)

func TestEnsureHomeRepository(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(t *testing.T, f *fixture) string
		// wantRepositories is how many repositories the registry holds after.
		wantRepositories int
		// wantOpenMain is whether the home repository's main worktree is an
		// open workspace after.
		wantOpenMain bool
		wantErr      error
	}{
		{
			name: "an empty registry gains agent-repl's repository with its main worktree open",
			arrange: func(t *testing.T, f *fixture) string {
				dir := worktreeDir(t)
				f.git.mainWorktree = dir
				return dir
			},
			wantRepositories: 1,
			wantOpenMain:     true,
		},
		{
			name: "a registry already holding it is left as it is",
			arrange: func(t *testing.T, f *fixture) string {
				dir := worktreeDir(t)
				f.git.mainWorktree = dir
				if _, err := f.verbs.RegisterRepository(context.Background(), dir); err != nil {
					t.Fatalf("RegisterRepository: %v", err)
				}
				return dir
			},
			wantRepositories: 1,
			wantOpenMain:     true,
		},
		{
			name:             "an empty checkout registers nothing",
			arrange:          func(*testing.T, *fixture) string { return "" },
			wantRepositories: 0,
		},
		{
			name: "a checkout in no repository registers nothing",
			arrange: func(t *testing.T, f *fixture) string {
				f.git.outsideEveryRepository = true
				return t.TempDir()
			},
			wantRepositories: 0,
		},
		{
			name: "a git failure is surfaced",
			arrange: func(t *testing.T, f *fixture) string {
				f.git.repositoryOfErr = errFake
				return t.TempDir()
			},
			wantRepositories: 0,
			wantErr:          errFake,
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			f := newFixture(t)
			dir := tc.arrange(t, f)

			// Act
			err := f.verbs.EnsureHomeRepository(context.Background(), dir)

			// Assert
			if !errors.Is(err, tc.wantErr) || (tc.wantErr == nil && err != nil) {
				t.Fatalf("EnsureHomeRepository = %v, want %v", err, tc.wantErr)
			}
			repositories, err := f.db.ListRepositories(context.Background())
			if err != nil {
				t.Fatalf("ListRepositories: %v", err)
			}
			if len(repositories) != tc.wantRepositories {
				t.Fatalf("registry holds %d repositories, want %d", len(repositories), tc.wantRepositories)
			}
			if !tc.wantOpenMain {
				return
			}
			workspaces, err := f.db.ListWorkspaces(context.Background())
			if err != nil {
				t.Fatalf("ListWorkspaces: %v", err)
			}
			for _, ws := range workspaces {
				if ws.Dir == dir && !ws.Closed {
					return
				}
			}
			t.Fatalf("workspaces = %+v, want the main worktree %s open", workspaces, dir)
		})
	}
}
