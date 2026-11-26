package gitclient

import (
	"context"
	"fmt"
)

// WorktreeLister is what finding a branch's worktree asks of git.
type WorktreeLister interface {
	MainWorktree(ctx context.Context, dir string) (string, error)
	ListWorktrees(ctx context.Context, repoDir string) ([]Worktree, error)
}

// WorktreeHolding answers the worktree of the repository holding IN that has
// BRANCH checked out (git checks a branch out in at most one), with the
// repository's main worktree, and false when none does. It is the ONE
// LOOKUP of a branch's worktree: a merge's rule check and subject, and a
// create cut from a source branch, all ask it.
func WorktreeHolding(ctx context.Context, git WorktreeLister, in, branch string) (dir, main string, held bool, err error) {
	main, err = git.MainWorktree(ctx, in)
	if err != nil {
		return "", "", false, fmt.Errorf("gitclient: resolving the repository's main worktree: %w", err)
	}
	worktrees, err := git.ListWorktrees(ctx, main)
	if err != nil {
		return "", "", false, fmt.Errorf("gitclient: listing the repository's worktrees: %w", err)
	}
	for _, wt := range worktrees {
		if !wt.Bare && wt.Branch == branch {
			return wt.Dir, main, true, nil
		}
	}
	return "", main, false, nil
}
