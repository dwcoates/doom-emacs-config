package gitlayout

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// repo lays out a main worktree with one linked worktree, as git writes them,
// and answers the main worktree's and the linked one's directories.
func repo(t *testing.T) (string, string) {
	t.Helper()
	root := t.TempDir()
	main := filepath.Join(root, "main")
	linked := filepath.Join(root, "linked")
	admin := filepath.Join(main, ".git", "worktrees", "linked")
	for _, dir := range []string{admin, linked} {
		if err := os.MkdirAll(dir, 0o755); err != nil {
			t.Fatal(err)
		}
	}
	if err := os.WriteFile(filepath.Join(linked, ".git"), []byte("gitdir: "+admin+"\n"), 0o644); err != nil {
		t.Fatal(err)
	}
	return main, linked
}

func TestLinkedWorktreeOf(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(t *testing.T) (dir, repoDir string)
		want    string
	}{
		{name: "a linked worktree of the repository", arrange: func(t *testing.T) (string, string) {
			main, linked := repo(t)
			return linked, main
		}},
		{name: "a directory with no .git", arrange: func(t *testing.T) (string, string) {
			main, _ := repo(t)
			return t.TempDir(), main
		}, want: "is no linked git worktree"},
		{name: "a .git file naming no gitdir", arrange: func(t *testing.T) (string, string) {
			main, linked := repo(t)
			if err := os.WriteFile(filepath.Join(linked, ".git"), []byte("nonsense\n"), 0o644); err != nil {
				t.Fatal(err)
			}
			return linked, main
		}, want: "names no gitdir"},
		{name: "a worktree of another repository", arrange: func(t *testing.T) (string, string) {
			_, linked := repo(t)
			other, _ := repo(t)
			return linked, other
		}, want: "is a worktree of another repository"},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			dir, repoDir := tt.arrange(t)

			// Act
			err := LinkedWorktreeOf(dir, repoDir)

			// Assert
			if tt.want == "" {
				if err != nil {
					t.Fatalf("LinkedWorktreeOf = %v, want nil", err)
				}
				return
			}
			if err == nil || !strings.Contains(err.Error(), tt.want) {
				t.Fatalf("LinkedWorktreeOf = %v, want an error saying %q", err, tt.want)
			}
		})
	}
}
