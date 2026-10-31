package wsm

import (
	"os"
	"path/filepath"
	"regexp"
	"strings"
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

// TestNoSiteHandRollsTheMainWorktreeRule pins that the registry and the
// workspace verbs ask IsMainWorktree (or isMainWorkspace, its in-transaction
// reading) rather than comparing a directory with a repository directory
// themselves, in Go or in SQL.
func TestNoSiteHandRollsTheMainWorktreeRule(t *testing.T) {
	// Arrange
	handRolled := regexp.MustCompile(`(==|!=)\s*repoDir\b|\brepoDir\s*(==|!=)|\bdir\s*=\s*repositories\.dir\b`)
	var offenders []string

	// Act
	for _, dir := range []string{".", filepath.Join("..", "workspace")} {
		files, err := filepath.Glob(filepath.Join(dir, "*.go"))
		if err != nil {
			t.Fatalf("glob %s: %v", dir, err)
		}
		for _, file := range files {
			if strings.HasSuffix(file, "_test.go") || filepath.Base(file) == "mainworktree.go" {
				continue
			}
			src, err := os.ReadFile(file)
			if err != nil {
				t.Fatalf("read %s: %v", file, err)
			}
			if handRolled.Match(src) {
				offenders = append(offenders, file)
			}
		}
	}

	// Assert
	if len(offenders) != 0 {
		t.Fatalf("these files compare a directory with repoDir by hand; use IsMainWorktree: %v", offenders)
	}
}
