// Package gitclient is the daemon's git leaf. It knows no other daemon package.
//
// Every invocation is `git -C dir ...` with the inherited GIT_DIR,
// GIT_WORK_TREE, GIT_INDEX_FILE, GIT_COMMON_DIR, GIT_PREFIX,
// GIT_OBJECT_DIRECTORY and GIT_ALTERNATE_OBJECT_DIRECTORIES STRIPPED — a
// leaked GIT_DIR is a real, previously-observed source of bogus work-tree
// errors. Operations are local only: the client never fetches and never
// pushes. Every failure carries git's stdout and stderr as evidence.
// See ARCHITECTURE.md "gitclient".
package gitclient

import (
	"context"
	"fmt"
	"strings"
	"time"

	"claude-repld/internal/dlog"
	"claude-repld/internal/notimpl"
)

// Git is the leaf's whole surface.
type Git interface {
	// DefaultBranch reports the repository's default branch.
	DefaultBranch(ctx context.Context, repoDir string) (string, error)
	// ResolveRef resolves a ref to a full sha.
	ResolveRef(ctx context.Context, repoDir, ref string) (string, error)
	// CreateWorktree creates branch at baseRef and checks it out at
	// worktreeDir.
	CreateWorktree(ctx context.Context, repoDir, branch, baseRef, worktreeDir string) error
	// RemoveWorktree removes a worktree, leaving its branch.
	RemoveWorktree(ctx context.Context, repoDir, worktreeDir string) error
	// Nuke force-removes both the worktree and the branch. This is data
	// destruction and has no undo.
	Nuke(ctx context.Context, repoDir, worktreeDir, branch string) error
	// CommonDir reports a directory's repository common dir, canonicalized
	// with symlinks resolved. It is the repository's identity.
	CommonDir(ctx context.Context, dir string) (string, error)
	// SameRepo reports whether two directories belong to one repository. The
	// merge orchestrator keys its two methods on it.
	SameRepo(ctx context.Context, a, b string) (bool, error)
	// MergeNoFF merges sourceBranch into targetDir's checkout with --no-ff.
	// The outcome is an ANSWER: Landed with the merge commit, or Conflicted
	// with the conflicted files.
	MergeNoFF(ctx context.Context, targetDir, sourceBranch, message string) (MergeOutcome, error)
	// ConflictedFiles lists the paths currently in conflict.
	ConflictedFiles(ctx context.Context, dir string) ([]string, error)
	// AbortMerge aborts an in-progress merge.
	AbortMerge(ctx context.Context, dir string) error
	// RevertMerge reverts a landed merge commit.
	RevertMerge(ctx context.Context, targetDir, mergeCommit string) error
	// LandedRange lists what a merge commit brought in, walking its second
	// parent's history.
	LandedRange(ctx context.Context, targetDir, mergeCommit string) ([]Commit, error)
	// ChangedPaths lists the paths a range touched. The rollout controller
	// classifies subsystems from it.
	ChangedPaths(ctx context.Context, dir, rangeSpec string) ([]string, error)
	// IsClean reports whether the working tree and index are clean.
	IsClean(ctx context.Context, dir string) (bool, error)
	// CurrentBranch reports the checked-out branch.
	CurrentBranch(ctx context.Context, dir string) (string, error)
}

// MergeOutcome is a merge attempt's answer. Exactly one of Landed and
// Conflicted is set; neither is a failure.
type MergeOutcome struct {
	// Landed is the merge commit when the merge succeeded, nil otherwise.
	Landed *Commit
	// Conflicted lists the conflicted paths when the merge stopped, nil
	// otherwise.
	Conflicted []string
}

// Commit is one commit, as much of it as the daemon uses.
type Commit struct {
	// SHA is the full commit sha.
	SHA string
	// Subject is the first line of the message.
	Subject string
	// Author is the author's name.
	Author string
	// At is the commit's author time.
	At time.Time
}

// Error is a git invocation's failure, carrying the command's own output as
// evidence. Every Git method's error is one of these.
type Error struct {
	// Args is the argument vector, after the environment hygiene.
	Args []string
	// Dir is the -C directory.
	Dir string
	// ExitCode is git's exit status.
	ExitCode int
	// Stdout is git's stdout.
	Stdout string
	// Stderr is git's stderr.
	Stderr string
}

// Error carries git's own words, because the evidence is the point.
func (e *Error) Error() string {
	return fmt.Sprintf("git %s (in %s) exited %d: %s",
		strings.Join(e.Args, " "), e.Dir, e.ExitCode, strings.TrimSpace(e.Stderr))
}

// New builds the git client.
func New(log dlog.Surfaces) (Git, error) {
	return nil, notimpl.Err
}
