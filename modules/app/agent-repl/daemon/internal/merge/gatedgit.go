package merge

import (
	"context"
	"errors"
	"sync"
	"time"

	"claude-repld/internal/gitclient"
)

// This file is a run's GIT GATE: every git command a merge run makes goes
// through it, so the daemon's exit can wait for the one in flight to finish
// and start no other.
//
// THE DAEMON'S EXIT NEVER INTERRUPTS A MERGE'S GIT. A git killed halfway
// through a checkout, a rebase step or a fast-forward leaves a tree that is
// neither the state before the command nor the state after it, and the merge
// that resumes on the next boot could not tell which of its steps had
// happened. So the drain STOPS the gate -- no git starts after that -- and
// waits, within MergeGitStopBound, for the command already running to end of
// its own accord. Only then are the run's waits (a turn, the test gate) cut.
//
// A run's git calls are sequential (one run is one goroutine), so the gate
// holds at most one command in flight; it names it for the drain's record.

// errMergeStopping is what a gated git answers once the daemon's exit has
// stopped the gate: the command was never started. The run reads it as the
// exit, never as a failure of the merge.
var errMergeStopping = errors.New("merge: the daemon is exiting; no further git is started for this merge")

// MergeGitStopBound is how long the daemon's exit waits for a merge's git
// command already in flight to finish before the exit goes on without it.
//
// THE BOUND IS A LARGE MULTIPLE OF THE WORK IT COVERS. A merge's git commands
// are local and short -- one replayed commit, a merge commit in a scratch
// tree, a fast-forward checkout -- each well under a second on a healthy
// machine; the one network command (the merged-upstream fetch) is the long
// tail, and GIT_TERMINAL_PROMPT=0 keeps it from waiting on a credential. Ten
// seconds is an order of magnitude over the local commands. A command still
// running at the bound is recorded at ERROR, naming it: the next boot's
// resume then reads the tree that command left.
const MergeGitStopBound = 10 * time.Second

// gatedGit is one run's gate in front of the daemon's git.
type gatedGit struct {
	inner gitclient.Git

	mu sync.Mutex
	// stopping is set once by the drain; no command starts after it.
	stopping bool
	// inFlight names the command running now, empty when none is, and since
	// is when it started.
	inFlight string
	since    time.Time
	// idle is made by stop when a command is in flight, and closed by the
	// leave that ends it.
	idle chan struct{}
	now  func() time.Time
}

// newGatedGit builds a run's gate in front of inner.
func newGatedGit(inner gitclient.Git, now func() time.Time) *gatedGit {
	return &gatedGit{inner: inner, now: now}
}

// enter starts one command, or refuses it once the gate is stopped.
func (g *gatedGit) enter(name string) error {
	g.mu.Lock()
	defer g.mu.Unlock()
	if g.stopping {
		return errMergeStopping
	}
	g.inFlight, g.since = name, g.now()
	return nil
}

// leave ends the command enter started, and releases a drain waiting on it.
func (g *gatedGit) leave() {
	g.mu.Lock()
	defer g.mu.Unlock()
	g.inFlight = ""
	if g.idle != nil {
		close(g.idle)
		g.idle = nil
	}
}

// gitInFlight is the command a stopped gate was still running, for the
// drain's record.
type gitInFlight struct {
	name  string
	since time.Time
}

// stop stops the gate: no command starts after it. It answers a channel that
// closes once the command in flight (if any) has ended, and that command.
func (g *gatedGit) stop() (<-chan struct{}, gitInFlight) {
	g.mu.Lock()
	defer g.mu.Unlock()
	g.stopping = true
	if g.inFlight == "" {
		done := make(chan struct{})
		close(done)
		return done, gitInFlight{}
	}
	if g.idle == nil {
		g.idle = make(chan struct{})
	}
	return g.idle, gitInFlight{name: g.inFlight, since: g.since}
}

// The gate stands in front of EVERY method of the leaf, which the compiler
// holds it to: a method added to gitclient.Git that is not gated here does
// not build.
var _ gitclient.Git = (*gatedGit)(nil)

func (g *gatedGit) DefaultBranch(ctx context.Context, repoDir string) (string, error) {
	if err := g.enter("DefaultBranch"); err != nil {
		return "", err
	}
	defer g.leave()
	return g.inner.DefaultBranch(ctx, repoDir)
}

func (g *gatedGit) ResolveRef(ctx context.Context, repoDir, ref string) (string, error) {
	if err := g.enter("ResolveRef"); err != nil {
		return "", err
	}
	defer g.leave()
	return g.inner.ResolveRef(ctx, repoDir, ref)
}

func (g *gatedGit) BranchExists(ctx context.Context, repoDir, branch string) (bool, error) {
	if err := g.enter("BranchExists"); err != nil {
		return false, err
	}
	defer g.leave()
	return g.inner.BranchExists(ctx, repoDir, branch)
}

func (g *gatedGit) CreateWorktree(ctx context.Context, repoDir, branch, baseRef, worktreeDir string) error {
	if err := g.enter("CreateWorktree"); err != nil {
		return err
	}
	defer g.leave()
	return g.inner.CreateWorktree(ctx, repoDir, branch, baseRef, worktreeDir)
}

func (g *gatedGit) RestoreWorktree(ctx context.Context, repoDir, worktreeDir, branch string) error {
	if err := g.enter("RestoreWorktree"); err != nil {
		return err
	}
	defer g.leave()
	return g.inner.RestoreWorktree(ctx, repoDir, worktreeDir, branch)
}

func (g *gatedGit) UnregisterMissingWorktree(ctx context.Context, repoDir, worktreeDir string) error {
	if err := g.enter("UnregisterMissingWorktree"); err != nil {
		return err
	}
	defer g.leave()
	return g.inner.UnregisterMissingWorktree(ctx, repoDir, worktreeDir)
}

func (g *gatedGit) RemoveWorktree(ctx context.Context, repoDir, worktreeDir string) error {
	if err := g.enter("RemoveWorktree"); err != nil {
		return err
	}
	defer g.leave()
	return g.inner.RemoveWorktree(ctx, repoDir, worktreeDir)
}

func (g *gatedGit) AddDetachedWorktree(ctx context.Context, repoDir, worktreeDir, commit string) error {
	if err := g.enter("AddDetachedWorktree"); err != nil {
		return err
	}
	defer g.leave()
	return g.inner.AddDetachedWorktree(ctx, repoDir, worktreeDir, commit)
}

func (g *gatedGit) FastForward(ctx context.Context, dir, commit string) error {
	if err := g.enter("FastForward"); err != nil {
		return err
	}
	defer g.leave()
	return g.inner.FastForward(ctx, dir, commit)
}

func (g *gatedGit) IsAncestor(ctx context.Context, dir, ancestor, descendant string) (bool, error) {
	if err := g.enter("IsAncestor"); err != nil {
		return false, err
	}
	defer g.leave()
	return g.inner.IsAncestor(ctx, dir, ancestor, descendant)
}

func (g *gatedGit) Nuke(ctx context.Context, repoDir, worktreeDir, branch string) error {
	if err := g.enter("Nuke"); err != nil {
		return err
	}
	defer g.leave()
	return g.inner.Nuke(ctx, repoDir, worktreeDir, branch)
}

func (g *gatedGit) CommonDir(ctx context.Context, dir string) (string, error) {
	if err := g.enter("CommonDir"); err != nil {
		return "", err
	}
	defer g.leave()
	return g.inner.CommonDir(ctx, dir)
}

func (g *gatedGit) MainWorktree(ctx context.Context, dir string) (string, error) {
	if err := g.enter("MainWorktree"); err != nil {
		return "", err
	}
	defer g.leave()
	return g.inner.MainWorktree(ctx, dir)
}

func (g *gatedGit) RepositoryOf(ctx context.Context, dir string) (string, bool, error) {
	if err := g.enter("RepositoryOf"); err != nil {
		return "", false, err
	}
	defer g.leave()
	return g.inner.RepositoryOf(ctx, dir)
}

func (g *gatedGit) SameRepo(ctx context.Context, a, b string) (bool, error) {
	if err := g.enter("SameRepo"); err != nil {
		return false, err
	}
	defer g.leave()
	return g.inner.SameRepo(ctx, a, b)
}

func (g *gatedGit) MergeNoFF(ctx context.Context, targetDir, sourceBranch, message string) (gitclient.MergeOutcome, error) {
	if err := g.enter("MergeNoFF"); err != nil {
		return gitclient.MergeOutcome{}, err
	}
	defer g.leave()
	return g.inner.MergeNoFF(ctx, targetDir, sourceBranch, message)
}

func (g *gatedGit) Commit(ctx context.Context, dir, message string) (string, error) {
	if err := g.enter("Commit"); err != nil {
		return "", err
	}
	defer g.leave()
	return g.inner.Commit(ctx, dir, message)
}

func (g *gatedGit) ConflictedFiles(ctx context.Context, dir string) ([]string, error) {
	if err := g.enter("ConflictedFiles"); err != nil {
		return nil, err
	}
	defer g.leave()
	return g.inner.ConflictedFiles(ctx, dir)
}

func (g *gatedGit) AbortMerge(ctx context.Context, dir string) error {
	if err := g.enter("AbortMerge"); err != nil {
		return err
	}
	defer g.leave()
	return g.inner.AbortMerge(ctx, dir)
}

func (g *gatedGit) RevertMerge(ctx context.Context, targetDir, mergeCommit string) error {
	if err := g.enter("RevertMerge"); err != nil {
		return err
	}
	defer g.leave()
	return g.inner.RevertMerge(ctx, targetDir, mergeCommit)
}

func (g *gatedGit) LandedRange(ctx context.Context, targetDir, mergeCommit string) ([]gitclient.Commit, error) {
	if err := g.enter("LandedRange"); err != nil {
		return nil, err
	}
	defer g.leave()
	return g.inner.LandedRange(ctx, targetDir, mergeCommit)
}

func (g *gatedGit) ChangedPaths(ctx context.Context, dir, rangeSpec string) ([]string, error) {
	if err := g.enter("ChangedPaths"); err != nil {
		return nil, err
	}
	defer g.leave()
	return g.inner.ChangedPaths(ctx, dir, rangeSpec)
}

func (g *gatedGit) IsClean(ctx context.Context, dir string) (bool, error) {
	if err := g.enter("IsClean"); err != nil {
		return false, err
	}
	defer g.leave()
	return g.inner.IsClean(ctx, dir)
}

func (g *gatedGit) CurrentBranch(ctx context.Context, dir string) (string, error) {
	if err := g.enter("CurrentBranch"); err != nil {
		return "", err
	}
	defer g.leave()
	return g.inner.CurrentBranch(ctx, dir)
}

func (g *gatedGit) PathClean(ctx context.Context, dir, path string) (bool, error) {
	if err := g.enter("PathClean"); err != nil {
		return false, err
	}
	defer g.leave()
	return g.inner.PathClean(ctx, dir, path)
}

func (g *gatedGit) CommitPath(ctx context.Context, dir, path, message string) (string, error) {
	if err := g.enter("CommitPath"); err != nil {
		return "", err
	}
	defer g.leave()
	return g.inner.CommitPath(ctx, dir, path, message)
}

func (g *gatedGit) ListWorktrees(ctx context.Context, repoDir string) ([]gitclient.Worktree, error) {
	if err := g.enter("ListWorktrees"); err != nil {
		return nil, err
	}
	defer g.leave()
	return g.inner.ListWorktrees(ctx, repoDir)
}

func (g *gatedGit) PruneWorktrees(ctx context.Context, repoDir string) error {
	if err := g.enter("PruneWorktrees"); err != nil {
		return err
	}
	defer g.leave()
	return g.inner.PruneWorktrees(ctx, repoDir)
}

func (g *gatedGit) RemoveCleanWorktree(ctx context.Context, repoDir, worktreeDir string) error {
	if err := g.enter("RemoveCleanWorktree"); err != nil {
		return err
	}
	defer g.leave()
	return g.inner.RemoveCleanWorktree(ctx, repoDir, worktreeDir)
}

func (g *gatedGit) AdminDir(ctx context.Context, worktreeDir string) (string, error) {
	if err := g.enter("AdminDir"); err != nil {
		return "", err
	}
	defer g.leave()
	return g.inner.AdminDir(ctx, worktreeDir)
}

func (g *gatedGit) CommitterTime(ctx context.Context, dir, ref string) (time.Time, error) {
	if err := g.enter("CommitterTime"); err != nil {
		return time.Time{}, err
	}
	defer g.leave()
	return g.inner.CommitterTime(ctx, dir, ref)
}

func (g *gatedGit) TreeOf(ctx context.Context, dir, ref string) (string, error) {
	if err := g.enter("TreeOf"); err != nil {
		return "", err
	}
	defer g.leave()
	return g.inner.TreeOf(ctx, dir, ref)
}

func (g *gatedGit) MergeTree(ctx context.Context, dir, base, other string) (gitclient.MergeTreeOutcome, error) {
	if err := g.enter("MergeTree"); err != nil {
		return gitclient.MergeTreeOutcome{}, err
	}
	defer g.leave()
	return g.inner.MergeTree(ctx, dir, base, other)
}

func (g *gatedGit) DeleteBranchAt(ctx context.Context, repoDir, branch, head string) error {
	if err := g.enter("DeleteBranchAt"); err != nil {
		return err
	}
	defer g.leave()
	return g.inner.DeleteBranchAt(ctx, repoDir, branch, head)
}

func (g *gatedGit) PreserveWorktree(ctx context.Context, repoDir, worktreeDir, ref, message string) (string, error) {
	if err := g.enter("PreserveWorktree"); err != nil {
		return "", err
	}
	defer g.leave()
	return g.inner.PreserveWorktree(ctx, repoDir, worktreeDir, ref, message)
}

func (g *gatedGit) CommitsBetween(ctx context.Context, dir, base, tip string) ([]gitclient.Commit, error) {
	if err := g.enter("CommitsBetween"); err != nil {
		return nil, err
	}
	defer g.leave()
	return g.inner.CommitsBetween(ctx, dir, base, tip)
}

func (g *gatedGit) StartRebase(ctx context.Context, dir, onto string, commits []string) (gitclient.RebaseStep, error) {
	if err := g.enter("StartRebase"); err != nil {
		return gitclient.RebaseStep{}, err
	}
	defer g.leave()
	return g.inner.StartRebase(ctx, dir, onto, commits)
}

func (g *gatedGit) ContinueRebase(ctx context.Context, dir string) (gitclient.RebaseStep, error) {
	if err := g.enter("ContinueRebase"); err != nil {
		return gitclient.RebaseStep{}, err
	}
	defer g.leave()
	return g.inner.ContinueRebase(ctx, dir)
}

func (g *gatedGit) RebaseInProgress(ctx context.Context, dir string) (bool, error) {
	if err := g.enter("RebaseInProgress"); err != nil {
		return false, err
	}
	defer g.leave()
	return g.inner.RebaseInProgress(ctx, dir)
}

func (g *gatedGit) AddWorktree(ctx context.Context, repoDir, worktreeDir, branch string) error {
	if err := g.enter("AddWorktree"); err != nil {
		return err
	}
	defer g.leave()
	return g.inner.AddWorktree(ctx, repoDir, worktreeDir, branch)
}

func (g *gatedGit) Fetch(ctx context.Context, dir, remote string) error {
	if err := g.enter("Fetch"); err != nil {
		return err
	}
	defer g.leave()
	return g.inner.Fetch(ctx, dir, remote)
}
