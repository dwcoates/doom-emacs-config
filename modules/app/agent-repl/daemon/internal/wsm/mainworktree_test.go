package wsm

import (
	"testing"
)

func TestIsMainWorktree(t *testing.T) {
	tests := []struct {
		name         string
		workspaceDir string
		repoDir      string
		want         bool
	}{
		{name: "the repository's own directory is its main worktree", workspaceDir: "/src/repo", repoDir: "/src/repo", want: true},
		{name: "a sibling linked worktree is not", workspaceDir: "/src/repo-feature", repoDir: "/src/repo", want: false},
		{name: "a directory inside the main worktree is not", workspaceDir: "/src/repo/sub", repoDir: "/src/repo", want: false},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Act
			got := IsMainWorktree(tt.workspaceDir, tt.repoDir)

			// Assert
			if got != tt.want {
				t.Fatalf("IsMainWorktree(%q, %q) = %v, want %v", tt.workspaceDir, tt.repoDir, got, tt.want)
			}
		})
	}
}

func TestMainWorkspaceName(t *testing.T) {
	// Arrange
	repository := "doom"

	// Act
	got := MainWorkspaceName(repository)

	// Assert
	if got != "main (doom)" {
		t.Fatalf("MainWorkspaceName(%q) = %q, want %q", repository, got, "main (doom)")
	}
}
