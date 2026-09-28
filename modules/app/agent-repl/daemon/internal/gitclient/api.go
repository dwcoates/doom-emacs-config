// Package gitclient is the daemon's git leaf. It knows no other daemon package.
//
// Every invocation is `git -C dir ...` with the inherited GIT_DIR,
// GIT_WORK_TREE, GIT_INDEX_FILE, GIT_COMMON_DIR, GIT_PREFIX,
// GIT_OBJECT_DIRECTORY and GIT_ALTERNATE_OBJECT_DIRECTORIES STRIPPED — a
// leaked GIT_DIR is a real, previously-observed source of bogus work-tree
// errors. The hook markers (MergeQueueMarker, OwnerOverride) are stripped
// too, and only FastForward sets the queue's marker. Operations are local only: the client never fetches and never
// pushes. Every failure carries git's stdout and stderr as evidence.
// See ARCHITECTURE.md "gitclient".
package gitclient

import (
	"context"
	"errors"
	"fmt"
	"strings"
	"time"

	"claude-repld/internal/dlog"
)

// Git is the leaf's whole surface.
type Git interface {
	// DefaultBranch reports the repository's default branch.
	DefaultBranch(ctx context.Context, repoDir string) (string, error)
	// ResolveRef resolves a ref to a full sha.
	ResolveRef(ctx context.Context, repoDir, ref string) (string, error)
	// BranchExists reports whether a LOCAL branch by that name exists. It is
	// a PROBE, not a resolution: a branch that is not there is an ordinary
	// answer and never an error record, which is what a naming collision
	// check asks of it.
	BranchExists(ctx context.Context, repoDir, branch string) (bool, error)
	// CreateWorktree creates branch at baseRef and checks it out at
	// worktreeDir.
	CreateWorktree(ctx context.Context, repoDir, branch, baseRef, worktreeDir string) error
	// RemoveWorktree removes a worktree, leaving its branch.
	RemoveWorktree(ctx context.Context, repoDir, worktreeDir string) error
	// AddDetachedWorktree checks commit out at worktreeDir on a DETACHED HEAD,
	// creating no branch. It is the merge queue's own scratch tree: the merge
	// is made and tested there, and nothing names it but its directory.
	AddDetachedWorktree(ctx context.Context, repoDir, worktreeDir, commit string) error
	// FastForward moves dir's checked-out branch, and its tree, forward to
	// commit. It refuses anything that is not a fast-forward.
	FastForward(ctx context.Context, dir, commit string) error
	// IsAncestor reports whether ancestor is reachable from descendant. It is
	// a PROBE: "no" is an ordinary answer, and only a git that could not tell
	// is an error.
	IsAncestor(ctx context.Context, dir, ancestor, descendant string) (bool, error)
	// Nuke force-removes both the worktree and the branch. This is data
	// destruction and has no undo.
	Nuke(ctx context.Context, repoDir, worktreeDir, branch string) error
	// CommonDir reports a directory's repository common dir, canonicalized
	// with symlinks resolved. It is the repository's identity.
	CommonDir(ctx context.Context, dir string) (string, error)
	// MainWorktree reports a directory's repository's MAIN WORKTREE,
	// canonicalized. It is what workspace.v1's RepositoryRef.dir means ("the
	// repository's normalized main-worktree directory") and what a top-level
	// workspace's merge targets. A bare repository has none, which is an
	// error, never an empty answer.
	MainWorktree(ctx context.Context, dir string) (string, error)
	// RepositoryOf reports the MAIN WORKTREE of the repository a directory is
	// inside, and whether it is inside one at all. It is a PROBE, not a
	// resolution: a directory outside every repository is an ORDINARY ANSWER
	// (false, nil error) and never an error record, which is what registering
	// a repository from a path the user picked asks of it. MainWorktree is the
	// resolution for a directory the caller already knows is a worktree; this
	// is the question "is it one, and whose".
	RepositoryOf(ctx context.Context, dir string) (string, bool, error)
	// SameRepo reports whether two directories belong to one repository. The
	// merge orchestrator keys its two methods on it.
	SameRepo(ctx context.Context, a, b string) (bool, error)
	// MergeNoFF merges sourceBranch into targetDir's checkout with --no-ff.
	// The outcome is an ANSWER: Landed with the merge commit, or Conflicted
	// with the conflicted files.
	MergeNoFF(ctx context.Context, targetDir, sourceBranch, message string) (MergeOutcome, error)
	// Commit records the staged index as a commit and answers its full sha.
	// It is how a RESOLVED conflict is completed: the resolution flow leaves
	// the merge's index staged, and this is the one call that closes it.
	Commit(ctx context.Context, dir, message string) (string, error)
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

	// ListWorktrees lists every worktree the repository registers, main first.
	ListWorktrees(ctx context.Context, repoDir string) ([]Worktree, error)
	// PruneWorktrees retires the registrations whose directories are gone.
	PruneWorktrees(ctx context.Context, repoDir string) error
	// RemoveCleanWorktree removes a worktree WITHOUT --force, leaving its
	// branch: git refuses a tree with modified or untracked content.
	RemoveCleanWorktree(ctx context.Context, repoDir, worktreeDir string) error
	// AdminDir reports a worktree's own git directory.
	AdminDir(ctx context.Context, worktreeDir string) (string, error)
	// CommitterTime reports a commit's committer time.
	CommitterTime(ctx context.Context, dir, ref string) (time.Time, error)
	// TreeOf resolves a ref to the tree it records.
	TreeOf(ctx context.Context, dir, ref string) (string, error)
	// MergeTree computes the tree merging other into base would record,
	// touching no worktree, index or ref.
	MergeTree(ctx context.Context, dir, base, other string) (MergeTreeOutcome, error)
	// DeleteBranchAt deletes a local branch only while it still points at
	// head.
	DeleteBranchAt(ctx context.Context, repoDir, branch, head string) error
}

// Worktree is one entry of `git worktree list --porcelain`.
type Worktree struct {
	// Dir is the worktree's directory exactly as git printed it.
	Dir string
	// Head is the checked-out commit, empty for a bare entry.
	Head string
	// Branch is the checked-out branch's short name, empty when HEAD is
	// detached and for a bare entry.
	Branch string
	// Detached reports a detached HEAD.
	Detached bool
	// Bare reports the bare repository's own entry.
	Bare bool
	// Locked reports `git worktree lock`; LockedReason is its reason, empty
	// when none was given.
	Locked       bool
	LockedReason string
	// Prunable reports a registration `git worktree prune` would retire;
	// PrunableReason is git's reason.
	Prunable       bool
	PrunableReason string
}

// MergeTreeOutcome is MergeTree's answer.
type MergeTreeOutcome struct {
	// Tree is the tree the merge would record. A conflicted merge still has
	// one, with conflict markers in it.
	Tree string
	// Conflicted reports that the merge would stop on conflicts.
	Conflicted bool
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
// evidence. Every failing GIT INVOCATION surfaces as one of these; the two
// failures that are not a git invocation's — a repository with no determinable
// default branch, and git output this client could not parse — are ordinary
// errors, because there is no command whose exit status and stderr they could
// honestly report.
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
	// Signal names the signal that killed git, when a signal did rather than
	// git deciding anything. It is empty for every ordinary failure. A
	// signalled git has NO exit status (ExitCode is -1) and wrote no stderr,
	// so this is the only evidence such a failure carries.
	Signal string
}

// Error carries git's own words, because the evidence is the point — or, for a
// git nobody let finish, the signal that ended it, because there are no words.
func (e *Error) Error() string {
	if e.Signal != "" {
		return fmt.Sprintf("git %s (in %s) was killed by %s",
			strings.Join(e.Args, " "), e.Dir, e.Signal)
	}
	return fmt.Sprintf("git %s (in %s) exited %d: %s",
		strings.Join(e.Args, " "), e.Dir, e.ExitCode, strings.TrimSpace(e.Stderr))
}

// Cancelled is a git invocation THE DAEMON ITSELF ended: its context was
// cancelled or its deadline passed, and the process was killed as a result. It
// is deliberately NOT an *Error, because there is no honest exit status to
// report — git did not decide anything, we stopped it — and a shutdown that
// logged "git exited nonzero, exit -1" was false evidence about git.
//
// It unwraps to context.Canceled or context.DeadlineExceeded, so every caller
// that already asks errors.Is(err, context.Canceled) recognizes it unchanged.
type Cancelled struct {
	// Args is the argument vector, after the environment hygiene.
	Args []string
	// Dir is the -C directory.
	Dir string
	// Cause is the context's own error: context.Canceled or
	// context.DeadlineExceeded.
	Cause error
}

// Error names the subcommand we stopped and why, never an exit status.
func (c *Cancelled) Error() string {
	return fmt.Sprintf("git %s (in %s) was cancelled: %v",
		strings.Join(c.Args, " "), c.Dir, c.Cause)
}

// Unwrap exposes the context error, which is what callers classify on.
func (c *Cancelled) Unwrap() error { return c.Cause }

// Subcommand is the git subcommand that was stopped, for the log record.
func (c *Cancelled) Subcommand() string {
	if len(c.Args) == 0 {
		return ""
	}
	return c.Args[0]
}

// IsCancelled reports whether err is a git invocation this daemon stopped
// rather than one that failed. It is the one classification callers need, so
// none of them has to know the concrete type.
func IsCancelled(err error) bool {
	var cancelled *Cancelled
	return errors.As(err, &cancelled)
}

// New builds the git client. It holds no state beyond its log surfaces: every
// method's truth is the repository on disk, read fresh each time.
func New(log dlog.Surfaces) (Git, error) {
	if log == nil {
		return nil, errors.New("gitclient: log surfaces are required")
	}
	return &client{log: log}, nil
}
