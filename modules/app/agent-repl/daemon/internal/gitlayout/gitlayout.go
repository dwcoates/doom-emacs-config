// Package gitlayout reads what git itself wrote on disk about a repository's
// worktrees, with no git process: a linked worktree's `.git` is a FILE naming
// its administrative directory, `<repository>/.git/worktrees/<name>`.
package gitlayout

import (
	"fmt"
	"os"
	"path/filepath"
	"strings"
)

// LinkedWorktreeOf answers nil when DIR is a linked git worktree of the
// repository whose main worktree is REPODIR, and why not otherwise. It reads
// what git itself wrote: a linked worktree's `.git` is a FILE naming its
// administrative directory, `<repository>/.git/worktrees/<name>`.
func LinkedWorktreeOf(dir, repoDir string) error {
	data, err := os.ReadFile(filepath.Join(dir, ".git"))
	if err != nil {
		return fmt.Errorf("%s is no linked git worktree: %v", dir, err)
	}
	gitdir, ok := strings.CutPrefix(strings.TrimSpace(string(data)), "gitdir: ")
	if !ok {
		return fmt.Errorf("%s is no linked git worktree: its .git file names no gitdir", dir)
	}
	admin, err := filepath.EvalSymlinks(filepath.Dir(gitdir))
	if err != nil {
		return fmt.Errorf("%s is no linked git worktree: its gitdir %s cannot be resolved: %v", dir, gitdir, err)
	}
	worktrees, err := filepath.EvalSymlinks(filepath.Join(repoDir, ".git", "worktrees"))
	if err != nil {
		return fmt.Errorf("the repository at %s holds no linked worktrees: %v", repoDir, err)
	}
	if admin != worktrees {
		return fmt.Errorf("%s is a worktree of another repository (its gitdir is %s), not of %s", dir, gitdir, repoDir)
	}
	return nil
}
