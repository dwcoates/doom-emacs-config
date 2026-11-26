package merge

import (
	"context"
	"errors"
	"fmt"
	"sync"
	"time"

	"agent-repld/internal/gitclient"
)

// This file is a run's GIT GATE: every git command a merge run makes goes
// through it, and it is the one place that decides what that command runs
// under. The daemon's exit waits on it for the command in flight to finish
// and start no other; a run's cancellation stops the run at it.
//
// A MERGE'S GIT COMMAND IS NEVER CUT. A git killed halfway through a
// checkout, a rebase step or a fast-forward leaves a tree that is neither the
// state before the command nor the state after it (2026-11-26: a workspace
// closed 7 ms into its merge's `git rebase -i` killed git between creating
// its state directory and writing its state, and the abandoned terminal's
// abort then met a rebase git could not abort). So the gate runs every
// command it starts on a context the run's cancellation cannot reach
// (context.WithoutCancel), and the command finishes whole: the tree is
// always one git left. What ends a run is the NEXT command, which the gate
// refuses:
//
//   - once the daemon's exit stopped the gate (errMergeStopping). The drain
//     stops the gate and waits, within MergeGitStopBound, for the command
//     already running to end of its own accord (awaitStoppingPoints); only
//     then are the run's waits (a turn, the test gate) cut.
//   - once the run's context ended -- an abandonment, a suspension, any
//     cancellation -- before the command started (*gitRefused, which
//     carries the context's error and its cause).
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
// machine, and a merge runs no network git. Ten seconds is an order of magnitude over the local commands. A command still
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

// gitRefused is what a gated git answers once the run's context has ended
// before the command started: the command was never started, so the tree is
// the one the previous command left whole. It unwraps to the context's error
// (context.Canceled) and to its cause (an *abandonment, a suspension's cause,
// errDaemonExiting), so a caller asking either reads it as it read a git the
// cancellation killed; the run's end classifies it as a stop (run.stopped).
type gitRefused struct {
	// name is the gated method refused.
	name string
	// err is the context's error, cause its cause (the same error when the
	// context was cancelled without one).
	err, cause error
}

func (r *gitRefused) Error() string {
	if r.cause == nil || r.cause == r.err {
		return fmt.Sprintf("merge: git %s was not started: the merge's run was cancelled (%v)", r.name, r.err)
	}
	return fmt.Sprintf("merge: git %s was not started: the merge's run was cancelled (%v: %v)", r.name, r.err, r.cause)
}

// Unwrap exposes the context's error and its cause.
func (r *gitRefused) Unwrap() []error {
	if r.cause == nil || r.cause == r.err {
		return []error{r.err}
	}
	return []error{r.err, r.cause}
}

// enter starts one command under CTX, the run's context, and answers the
// context the command runs under: CTX without its cancellation, so nothing
// the run's cancellation does cuts the command once it started. It refuses
// the command once the gate is stopped (errMergeStopping) or once CTX has
// ended (*gitRefused). A bound a gitclient method sets inside is its own and
// stands; no merge git call relies on a deadline of its caller's.
func (g *gatedGit) enter(ctx context.Context, name string) (context.Context, error) {
	g.mu.Lock()
	defer g.mu.Unlock()
	if g.stopping {
		return nil, errMergeStopping
	}
	if err := ctx.Err(); err != nil {
		return nil, &gitRefused{name: name, err: err, cause: context.Cause(ctx)}
	}
	g.inFlight, g.since = name, g.now()
	return context.WithoutCancel(ctx), nil
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
	ctx, err := g.enter(ctx, "DefaultBranch")
	if err != nil {
		return "", err
	}
	defer g.leave()
	return g.inner.DefaultBranch(ctx, repoDir)
}

func (g *gatedGit) RefreshPrimaryBranch(ctx context.Context, repoDir string) error {
	ctx, err := g.enter(ctx, "RefreshPrimaryBranch")
	if err != nil {
		return err
	}
	defer g.leave()
	return g.inner.RefreshPrimaryBranch(ctx, repoDir)
}

func (g *gatedGit) ResolveRef(ctx context.Context, repoDir, ref string) (string, error) {
	ctx, err := g.enter(ctx, "ResolveRef")
	if err != nil {
		return "", err
	}
	defer g.leave()
	return g.inner.ResolveRef(ctx, repoDir, ref)
}

func (g *gatedGit) ListRefs(ctx context.Context, repoDir, prefix string) ([]string, error) {
	ctx, err := g.enter(ctx, "ListRefs")
	if err != nil {
		return nil, err
	}
	defer g.leave()
	return g.inner.ListRefs(ctx, repoDir, prefix)
}

func (g *gatedGit) BranchExists(ctx context.Context, repoDir, branch string) (bool, error) {
	ctx, err := g.enter(ctx, "BranchExists")
	if err != nil {
		return false, err
	}
	defer g.leave()
	return g.inner.BranchExists(ctx, repoDir, branch)
}

func (g *gatedGit) RefExists(ctx context.Context, repoDir, ref string) (bool, error) {
	ctx, err := g.enter(ctx, "RefExists")
	if err != nil {
		return false, err
	}
	defer g.leave()
	return g.inner.RefExists(ctx, repoDir, ref)
}

func (g *gatedGit) CreateWorktree(ctx context.Context, repoDir, branch, baseRef, worktreeDir string) error {
	ctx, err := g.enter(ctx, "CreateWorktree")
	if err != nil {
		return err
	}
	defer g.leave()
	return g.inner.CreateWorktree(ctx, repoDir, branch, baseRef, worktreeDir)
}

func (g *gatedGit) RestoreWorktree(ctx context.Context, repoDir, worktreeDir, branch string) error {
	ctx, err := g.enter(ctx, "RestoreWorktree")
	if err != nil {
		return err
	}
	defer g.leave()
	return g.inner.RestoreWorktree(ctx, repoDir, worktreeDir, branch)
}

func (g *gatedGit) UnregisterMissingWorktree(ctx context.Context, repoDir, worktreeDir string) error {
	ctx, err := g.enter(ctx, "UnregisterMissingWorktree")
	if err != nil {
		return err
	}
	defer g.leave()
	return g.inner.UnregisterMissingWorktree(ctx, repoDir, worktreeDir)
}

func (g *gatedGit) RemoveWorktree(ctx context.Context, repoDir, worktreeDir string) error {
	ctx, err := g.enter(ctx, "RemoveWorktree")
	if err != nil {
		return err
	}
	defer g.leave()
	return g.inner.RemoveWorktree(ctx, repoDir, worktreeDir)
}

func (g *gatedGit) MoveWorktree(ctx context.Context, repoDir, from, to string) error {
	ctx, err := g.enter(ctx, "MoveWorktree")
	if err != nil {
		return err
	}
	defer g.leave()
	return g.inner.MoveWorktree(ctx, repoDir, from, to)
}

func (g *gatedGit) RenameBranch(ctx context.Context, repoDir, from, to string) error {
	ctx, err := g.enter(ctx, "RenameBranch")
	if err != nil {
		return err
	}
	defer g.leave()
	return g.inner.RenameBranch(ctx, repoDir, from, to)
}

func (g *gatedGit) AddDetachedWorktree(ctx context.Context, repoDir, worktreeDir, commit string) error {
	ctx, err := g.enter(ctx, "AddDetachedWorktree")
	if err != nil {
		return err
	}
	defer g.leave()
	return g.inner.AddDetachedWorktree(ctx, repoDir, worktreeDir, commit)
}

func (g *gatedGit) FastForward(ctx context.Context, dir, commit string) error {
	ctx, err := g.enter(ctx, "FastForward")
	if err != nil {
		return err
	}
	defer g.leave()
	return g.inner.FastForward(ctx, dir, commit)
}

func (g *gatedGit) TagsMerged(ctx context.Context, dir, commit string) ([]string, error) {
	ctx, err := g.enter(ctx, "TagsMerged")
	if err != nil {
		return nil, err
	}
	defer g.leave()
	return g.inner.TagsMerged(ctx, dir, commit)
}

func (g *gatedGit) TagsAt(ctx context.Context, dir, commit string) ([]string, error) {
	ctx, err := g.enter(ctx, "TagsAt")
	if err != nil {
		return nil, err
	}
	defer g.leave()
	return g.inner.TagsAt(ctx, dir, commit)
}

func (g *gatedGit) CreateAnnotatedTag(ctx context.Context, dir, name, commit, message string) error {
	ctx, err := g.enter(ctx, "CreateAnnotatedTag")
	if err != nil {
		return err
	}
	defer g.leave()
	return g.inner.CreateAnnotatedTag(ctx, dir, name, commit, message)
}

func (g *gatedGit) IsAncestor(ctx context.Context, dir, ancestor, descendant string) (bool, error) {
	ctx, err := g.enter(ctx, "IsAncestor")
	if err != nil {
		return false, err
	}
	defer g.leave()
	return g.inner.IsAncestor(ctx, dir, ancestor, descendant)
}

func (g *gatedGit) Nuke(ctx context.Context, repoDir, worktreeDir, branch string) error {
	ctx, err := g.enter(ctx, "Nuke")
	if err != nil {
		return err
	}
	defer g.leave()
	return g.inner.Nuke(ctx, repoDir, worktreeDir, branch)
}

func (g *gatedGit) CommonDir(ctx context.Context, dir string) (string, error) {
	ctx, err := g.enter(ctx, "CommonDir")
	if err != nil {
		return "", err
	}
	defer g.leave()
	return g.inner.CommonDir(ctx, dir)
}

func (g *gatedGit) MainWorktree(ctx context.Context, dir string) (string, error) {
	ctx, err := g.enter(ctx, "MainWorktree")
	if err != nil {
		return "", err
	}
	defer g.leave()
	return g.inner.MainWorktree(ctx, dir)
}

func (g *gatedGit) RepositoryOf(ctx context.Context, dir string) (string, bool, error) {
	ctx, err := g.enter(ctx, "RepositoryOf")
	if err != nil {
		return "", false, err
	}
	defer g.leave()
	return g.inner.RepositoryOf(ctx, dir)
}

func (g *gatedGit) MergeNoFF(ctx context.Context, targetDir, sourceBranch, message string) (gitclient.MergeOutcome, error) {
	ctx, err := g.enter(ctx, "MergeNoFF")
	if err != nil {
		return gitclient.MergeOutcome{}, err
	}
	defer g.leave()
	return g.inner.MergeNoFF(ctx, targetDir, sourceBranch, message)
}

func (g *gatedGit) Commit(ctx context.Context, dir, message string) (string, error) {
	ctx, err := g.enter(ctx, "Commit")
	if err != nil {
		return "", err
	}
	defer g.leave()
	return g.inner.Commit(ctx, dir, message)
}

func (g *gatedGit) ConflictedFiles(ctx context.Context, dir string) ([]string, error) {
	ctx, err := g.enter(ctx, "ConflictedFiles")
	if err != nil {
		return nil, err
	}
	defer g.leave()
	return g.inner.ConflictedFiles(ctx, dir)
}

func (g *gatedGit) AbortMerge(ctx context.Context, dir string) error {
	ctx, err := g.enter(ctx, "AbortMerge")
	if err != nil {
		return err
	}
	defer g.leave()
	return g.inner.AbortMerge(ctx, dir)
}

func (g *gatedGit) RevertMerge(ctx context.Context, targetDir, mergeCommit string) error {
	ctx, err := g.enter(ctx, "RevertMerge")
	if err != nil {
		return err
	}
	defer g.leave()
	return g.inner.RevertMerge(ctx, targetDir, mergeCommit)
}

func (g *gatedGit) LandedRange(ctx context.Context, targetDir, mergeCommit string) ([]gitclient.Commit, error) {
	ctx, err := g.enter(ctx, "LandedRange")
	if err != nil {
		return nil, err
	}
	defer g.leave()
	return g.inner.LandedRange(ctx, targetDir, mergeCommit)
}

func (g *gatedGit) ChangedPaths(ctx context.Context, dir, rangeSpec string) ([]string, error) {
	ctx, err := g.enter(ctx, "ChangedPaths")
	if err != nil {
		return nil, err
	}
	defer g.leave()
	return g.inner.ChangedPaths(ctx, dir, rangeSpec)
}

func (g *gatedGit) IsClean(ctx context.Context, dir string) (bool, error) {
	ctx, err := g.enter(ctx, "IsClean")
	if err != nil {
		return false, err
	}
	defer g.leave()
	return g.inner.IsClean(ctx, dir)
}

func (g *gatedGit) CurrentBranch(ctx context.Context, dir string) (string, error) {
	ctx, err := g.enter(ctx, "CurrentBranch")
	if err != nil {
		return "", err
	}
	defer g.leave()
	return g.inner.CurrentBranch(ctx, dir)
}

func (g *gatedGit) PathClean(ctx context.Context, dir, path string) (bool, error) {
	ctx, err := g.enter(ctx, "PathClean")
	if err != nil {
		return false, err
	}
	defer g.leave()
	return g.inner.PathClean(ctx, dir, path)
}

func (g *gatedGit) CommitPath(ctx context.Context, dir, path, message string) (string, error) {
	ctx, err := g.enter(ctx, "CommitPath")
	if err != nil {
		return "", err
	}
	defer g.leave()
	return g.inner.CommitPath(ctx, dir, path, message)
}

func (g *gatedGit) ListWorktrees(ctx context.Context, repoDir string) ([]gitclient.Worktree, error) {
	ctx, err := g.enter(ctx, "ListWorktrees")
	if err != nil {
		return nil, err
	}
	defer g.leave()
	return g.inner.ListWorktrees(ctx, repoDir)
}

func (g *gatedGit) PruneWorktrees(ctx context.Context, repoDir string) error {
	ctx, err := g.enter(ctx, "PruneWorktrees")
	if err != nil {
		return err
	}
	defer g.leave()
	return g.inner.PruneWorktrees(ctx, repoDir)
}

func (g *gatedGit) RemoveCleanWorktree(ctx context.Context, repoDir, worktreeDir string) error {
	ctx, err := g.enter(ctx, "RemoveCleanWorktree")
	if err != nil {
		return err
	}
	defer g.leave()
	return g.inner.RemoveCleanWorktree(ctx, repoDir, worktreeDir)
}

func (g *gatedGit) AdminDir(ctx context.Context, worktreeDir string) (string, error) {
	ctx, err := g.enter(ctx, "AdminDir")
	if err != nil {
		return "", err
	}
	defer g.leave()
	return g.inner.AdminDir(ctx, worktreeDir)
}

func (g *gatedGit) CommitterTime(ctx context.Context, dir, ref string) (time.Time, error) {
	ctx, err := g.enter(ctx, "CommitterTime")
	if err != nil {
		return time.Time{}, err
	}
	defer g.leave()
	return g.inner.CommitterTime(ctx, dir, ref)
}

func (g *gatedGit) TreeOf(ctx context.Context, dir, ref string) (string, error) {
	ctx, err := g.enter(ctx, "TreeOf")
	if err != nil {
		return "", err
	}
	defer g.leave()
	return g.inner.TreeOf(ctx, dir, ref)
}

func (g *gatedGit) MergeTree(ctx context.Context, dir, base, other string) (gitclient.MergeTreeOutcome, error) {
	ctx, err := g.enter(ctx, "MergeTree")
	if err != nil {
		return gitclient.MergeTreeOutcome{}, err
	}
	defer g.leave()
	return g.inner.MergeTree(ctx, dir, base, other)
}

func (g *gatedGit) DeleteBranchAt(ctx context.Context, repoDir, branch, head string) error {
	ctx, err := g.enter(ctx, "DeleteBranchAt")
	if err != nil {
		return err
	}
	defer g.leave()
	return g.inner.DeleteBranchAt(ctx, repoDir, branch, head)
}

func (g *gatedGit) PreserveWorktree(ctx context.Context, repoDir, worktreeDir, ref, message string) (string, error) {
	ctx, err := g.enter(ctx, "PreserveWorktree")
	if err != nil {
		return "", err
	}
	defer g.leave()
	return g.inner.PreserveWorktree(ctx, repoDir, worktreeDir, ref, message)
}

func (g *gatedGit) CommitsBetween(ctx context.Context, dir, base, tip string) ([]gitclient.Commit, error) {
	ctx, err := g.enter(ctx, "CommitsBetween")
	if err != nil {
		return nil, err
	}
	defer g.leave()
	return g.inner.CommitsBetween(ctx, dir, base, tip)
}

func (g *gatedGit) GlobalIdentity(ctx context.Context, dir string) (gitclient.Identity, error) {
	ctx, err := g.enter(ctx, "GlobalIdentity")
	if err != nil {
		return gitclient.Identity{}, err
	}
	defer g.leave()
	return g.inner.GlobalIdentity(ctx, dir)
}

func (g *gatedGit) CommitIdentities(ctx context.Context, dir, base, tip string) ([]gitclient.CommitIdentity, error) {
	ctx, err := g.enter(ctx, "CommitIdentities")
	if err != nil {
		return nil, err
	}
	defer g.leave()
	return g.inner.CommitIdentities(ctx, dir, base, tip)
}

func (g *gatedGit) CountCommits(ctx context.Context, dir, base, tip string) (uint32, error) {
	ctx, err := g.enter(ctx, "CountCommits")
	if err != nil {
		return 0, err
	}
	defer g.leave()
	return g.inner.CountCommits(ctx, dir, base, tip)
}

func (g *gatedGit) StartRebase(ctx context.Context, dir, onto string, commits []string) (gitclient.RebaseStep, error) {
	ctx, err := g.enter(ctx, "StartRebase")
	if err != nil {
		return gitclient.RebaseStep{}, err
	}
	defer g.leave()
	return g.inner.StartRebase(ctx, dir, onto, commits)
}

func (g *gatedGit) ContinueRebase(ctx context.Context, dir string) (gitclient.RebaseStep, error) {
	ctx, err := g.enter(ctx, "ContinueRebase")
	if err != nil {
		return gitclient.RebaseStep{}, err
	}
	defer g.leave()
	return g.inner.ContinueRebase(ctx, dir)
}

func (g *gatedGit) AbortRebase(ctx context.Context, dir string) error {
	ctx, err := g.enter(ctx, "AbortRebase")
	if err != nil {
		return err
	}
	defer g.leave()
	return g.inner.AbortRebase(ctx, dir)
}

func (g *gatedGit) RestoreBranchHead(ctx context.Context, dir, head string) error {
	ctx, err := g.enter(ctx, "RestoreBranchHead")
	if err != nil {
		return err
	}
	defer g.leave()
	return g.inner.RestoreBranchHead(ctx, dir, head)
}

func (g *gatedGit) RebaseInProgress(ctx context.Context, dir string) (bool, error) {
	ctx, err := g.enter(ctx, "RebaseInProgress")
	if err != nil {
		return false, err
	}
	defer g.leave()
	return g.inner.RebaseInProgress(ctx, dir)
}

func (g *gatedGit) AddWorktree(ctx context.Context, repoDir, worktreeDir, branch string) error {
	ctx, err := g.enter(ctx, "AddWorktree")
	if err != nil {
		return err
	}
	defer g.leave()
	return g.inner.AddWorktree(ctx, repoDir, worktreeDir, branch)
}

func (g *gatedGit) FetchBranch(ctx context.Context, repoDir, remote, branch string) (string, error) {
	ctx, err := g.enter(ctx, "FetchBranch")
	if err != nil {
		return "", err
	}
	defer g.leave()
	return g.inner.FetchBranch(ctx, repoDir, remote, branch)
}

func (g *gatedGit) Upstream(ctx context.Context, repoDir, branch string) (gitclient.Upstream, bool, error) {
	ctx, err := g.enter(ctx, "Upstream")
	if err != nil {
		return gitclient.Upstream{}, false, err
	}
	defer g.leave()
	return g.inner.Upstream(ctx, repoDir, branch)
}

func (g *gatedGit) AheadBehind(ctx context.Context, dir, local, upstream string) (int, int, error) {
	ctx, err := g.enter(ctx, "AheadBehind")
	if err != nil {
		return 0, 0, err
	}
	defer g.leave()
	return g.inner.AheadBehind(ctx, dir, local, upstream)
}

func (g *gatedGit) AnyPatchEquivalent(ctx context.Context, dir, upstream, head string) (bool, error) {
	ctx, err := g.enter(ctx, "AnyPatchEquivalent")
	if err != nil {
		return false, err
	}
	defer g.leave()
	return g.inner.AnyPatchEquivalent(ctx, dir, upstream, head)
}

func (g *gatedGit) RemoteURL(ctx context.Context, repoDir, remote string) (string, bool, error) {
	ctx, err := g.enter(ctx, "RemoteURL")
	if err != nil {
		return "", false, err
	}
	defer g.leave()
	return g.inner.RemoteURL(ctx, repoDir, remote)
}
