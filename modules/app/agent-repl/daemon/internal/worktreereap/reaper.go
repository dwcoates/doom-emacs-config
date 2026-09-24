// Package worktreereap is the daemon's LANDED-WORKTREE REAPER: a low-priority
// background sweep that removes every linked worktree whose changes have
// already landed on its repository's default branch, and deletes its branch.
//
// The merge queue retires the worktrees it merges. This covers the ones it
// never saw -- worktrees agents cut for themselves, and branches that landed
// by a squash or a cherry-pick rather than through the queue.
//
// THE LANDED RULE IS PROGRAMMATIC and it is about CHANGES, not commits: a
// worktree has landed iff `git merge-tree --write-tree <default> <HEAD>`
// answers exactly the default branch's own tree -- merging the branch into the
// default branch would change nothing. That holds for a merge, a cherry-pick
// and a squash alike; a conflict or any difference is NOT landed.
//
// See daemon/AGENTS.md "The landed-worktree reaper" for the safety gates.
package worktreereap

import (
	"context"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"sync"
	"time"

	"claude-repld/internal/clock"
	"claude-repld/internal/dlog"
	"claude-repld/internal/gitclient"
	"claude-repld/internal/ids"
	"claude-repld/internal/wsm"
)

// The records this package writes.
const (
	opSweep  = "daemon.worktreereap.sweep"
	opRepo   = "daemon.worktreereap.repo"
	opPrune  = "daemon.worktreereap.prune"
	opKeep   = "daemon.worktreereap.keep"
	opJudge  = "daemon.worktreereap.judge"
	opRemove = "daemon.worktreereap.remove"
	opBranch = "daemon.worktreereap.branch"
	opRun    = "daemon.worktreereap.run"
)

// Production windows. See daemon/AGENTS.md "The landed-worktree reaper".
const (
	// DefaultIdleAfter is how long a worktree must have shown no activity
	// before it is eligible at all. A worktree cut a moment ago sits at the
	// default branch with no commits of its own, which is trivially "landed",
	// and an agent may be about to start in it.
	DefaultIdleAfter = 24 * time.Hour
	// DefaultStartDelay is how long after the daemon starts the first sweep
	// runs: long enough for the boot's adoptions and the editor's first
	// opens to settle, short enough that a daemon restarted every day still
	// sweeps every day.
	DefaultStartDelay = 5 * time.Minute
	// DefaultEvery is the cadence after the first sweep.
	DefaultEvery = 24 * time.Hour
)

// ErrSweepRunning is Sweep's refusal while another sweep of this reaper is in
// flight. The sweep never runs concurrently with itself.
var ErrSweepRunning = errors.New("worktreereap: a sweep is already running")

// Git is the part of the git leaf the reaper drives.
type Git interface {
	DefaultBranch(ctx context.Context, repoDir string) (string, error)
	ResolveRef(ctx context.Context, repoDir, ref string) (string, error)
	TreeOf(ctx context.Context, dir, ref string) (string, error)
	ListWorktrees(ctx context.Context, repoDir string) ([]gitclient.Worktree, error)
	PruneWorktrees(ctx context.Context, repoDir string) error
	AdminDir(ctx context.Context, worktreeDir string) (string, error)
	CommitterTime(ctx context.Context, dir, ref string) (time.Time, error)
	IsClean(ctx context.Context, dir string) (bool, error)
	MergeTree(ctx context.Context, dir, base, other string) (gitclient.MergeTreeOutcome, error)
	RemoveCleanWorktree(ctx context.Context, repoDir, worktreeDir string) error
	DeleteBranchAt(ctx context.Context, repoDir, branch, head string) error
}

// Registry is the part of the state client the reaper reads: the repositories
// whose worktrees the daemon manages, and the workspaces it knows.
type Registry interface {
	ListRepositories(ctx context.Context) ([]wsm.Repository, error)
	ListWorkspaces(ctx context.Context) ([]wsm.Workspace, error)
}

// Deps are the reaper's collaborators and windows.
type Deps struct {
	// Git is the git leaf.
	Git Git
	// Registry is the state client.
	Registry Registry
	// LiveSessions answers every workspace this daemon holds a live session
	// for (the fleet's in-memory map). It is read once per sweep.
	LiveSessions func() []ids.WorkspaceID
	// Clock is the reaper's view of time.
	Clock clock.Clock
	// LockPath is the kernel lock one sweep holds for its whole run, so two
	// daemons (an incumbent and its handover successor) never sweep at once.
	LockPath string
	// IdleAfter, StartDelay and Every are the windows; see the defaults.
	IdleAfter  time.Duration
	StartDelay time.Duration
	Every      time.Duration
	// Log is the global logger: most of what the reaper judges is no
	// workspace's.
	Log dlog.Logger
}

// Reaper is the landed-worktree reaper.
type Reaper struct {
	deps Deps
	// sweeping is held for one sweep's whole run; TryLock is the refusal.
	sweeping sync.Mutex
}

// New builds the reaper, refusing a missing collaborator or a non-positive
// window rather than running on a zero value.
func New(deps Deps) (*Reaper, error) {
	switch {
	case deps.Git == nil:
		return nil, errors.New("worktreereap: Git is required")
	case deps.Registry == nil:
		return nil, errors.New("worktreereap: Registry is required")
	case deps.LiveSessions == nil:
		return nil, errors.New("worktreereap: LiveSessions is required")
	case deps.Clock == nil:
		return nil, errors.New("worktreereap: Clock is required")
	case deps.LockPath == "":
		return nil, errors.New("worktreereap: LockPath is required")
	case deps.Log == nil:
		return nil, errors.New("worktreereap: Log is required")
	case deps.IdleAfter <= 0, deps.StartDelay <= 0, deps.Every <= 0:
		return nil, fmt.Errorf("worktreereap: the windows must be positive (idle after %v, start delay %v, every %v)",
			deps.IdleAfter, deps.StartDelay, deps.Every)
	}
	return &Reaper{deps: deps}, nil
}

// Keep reasons: why a worktree was left alone. They are the summary's keys.
const (
	keepMain           = "main_worktree"
	keepBare           = "bare"
	keepLocked         = "locked"
	keepPrunable       = "prunable"
	keepOpenWorkspace  = "open_workspace"
	keepLiveSession    = "live_session"
	keepParentBranch   = "parent_of_open_workspace"
	keepDefaultBranch  = "on_default_branch"
	keepMissing        = "directory_missing"
	keepNoCommit       = "no_commit"
	keepActive         = "recently_active"
	keepDirty          = "dirty"
	keepConflicts      = "merge_conflicts"
	keepUnlanded       = "unlanded"
	reasonRemoveFailed = "remove_failed"
)

// Report is one sweep's account.
type Report struct {
	// Repositories is how many repositories were swept to the end.
	Repositories int
	// Worktrees is how many linked worktrees were judged.
	Worktrees int
	// Removed lists every removed worktree directory.
	Removed []string
	// BranchesDeleted counts the branches deleted after their removal.
	BranchesDeleted int
	// Pruned counts the repositories whose stale registrations were pruned.
	Pruned int
	// Kept counts the worktrees left alone, by reason.
	Kept map[string]int
	// Failures counts every step that failed and was recorded at ERROR.
	Failures int
}

// Sweep runs one sweep over every repository the registry knows. It refuses
// with ErrSweepRunning while another sweep of this reaper is in flight, and it
// answers a zero report without sweeping while another PROCESS holds the
// sweep's kernel lock. A failure inside one repository or one worktree is
// recorded at ERROR and the sweep goes on; only a registry that cannot be read,
// or the daemon's own exit, ends it early.
func (r *Reaper) Sweep(ctx context.Context) (Report, error) {
	if !r.sweeping.TryLock() {
		r.deps.Log.Info(opSweep, "a sweep was asked for while one is running; it is not started", nil)
		return Report{}, ErrSweepRunning
	}
	defer r.sweeping.Unlock()

	lock, held, err := acquireLock(r.deps.LockPath)
	if err != nil {
		r.deps.Log.Error(opSweep, "the sweep's kernel lock could not be told about; nothing is swept", dlog.Context{
			"lock": r.deps.LockPath, "cause": err.Error(),
		})
		return Report{}, err
	}
	if !held {
		r.deps.Log.Info(opSweep, "another daemon is sweeping; this one does not", dlog.Context{"lock": r.deps.LockPath})
		return Report{}, nil
	}
	defer func() {
		if err := lock.Release(); err != nil {
			r.deps.Log.Error(opSweep, "the sweep's kernel lock could not be released", dlog.Context{
				"lock": r.deps.LockPath, "cause": err.Error(),
			})
		}
	}()

	now := r.deps.Clock.Now()
	report := Report{Kept: map[string]int{}}
	repos, err := r.deps.Registry.ListRepositories(ctx)
	if err != nil {
		return report, r.registryFailure(ctx, "the repositories could not be read; nothing is swept", err)
	}
	workspaces, err := r.deps.Registry.ListWorkspaces(ctx)
	if err != nil {
		return report, r.registryFailure(ctx, "the workspaces could not be read; nothing is swept", err)
	}
	known := newRegistryView(workspaces, r.deps.LiveSessions())

	r.deps.Log.Info(opSweep, "sweeping for landed worktrees", dlog.Context{
		"repositories": len(repos), "idle_after": r.deps.IdleAfter.String(),
	})
	for _, repo := range repos {
		if ctx.Err() != nil {
			break
		}
		(&repoSweep{r: r, repo: repo, known: known, now: now, report: &report}).run(ctx)
	}
	if ctx.Err() != nil {
		r.deps.Log.Info(opSweep, "the sweep stopped with the daemon", summary(report))
		return report, ctx.Err()
	}
	r.deps.Log.Info(opSweep, "the sweep finished", summary(report))
	return report, nil
}

// registryFailure records a registry read that ended the sweep: at INFO when
// the daemon's exit took it away, at ERROR otherwise.
func (r *Reaper) registryFailure(ctx context.Context, message string, err error) error {
	if ctx.Err() != nil {
		r.deps.Log.Info(opSweep, "the sweep stopped with the daemon", dlog.Context{"cause": err.Error()})
		return ctx.Err()
	}
	r.deps.Log.Error(opSweep, message, dlog.Context{"cause": err.Error()})
	return err
}

// summary is the sweep summary's context.
func summary(report Report) dlog.Context {
	return dlog.Context{
		"repositories":     report.Repositories,
		"worktrees":        report.Worktrees,
		"removed":          len(report.Removed),
		"removed_dirs":     report.Removed,
		"branches_deleted": report.BranchesDeleted,
		"pruned":           report.Pruned,
		"kept":             report.Kept,
		"failures":         report.Failures,
	}
}

// registryView is what the registry says about worktrees, snapshotted once per
// sweep.
type registryView struct {
	// byDir is every registered workspace, by canonical directory.
	byDir map[string]wsm.Workspace
	// live is the canonical directory of every workspace with a live session.
	live map[string]bool
	// parents is, per repository, every branch an OPEN workspace was cut
	// from: a nested workspace merges into its parent's worktree.
	parents map[ids.RepoID]map[string]bool
}

func newRegistryView(workspaces []wsm.Workspace, live []ids.WorkspaceID) registryView {
	view := registryView{
		byDir:   map[string]wsm.Workspace{},
		live:    map[string]bool{},
		parents: map[ids.RepoID]map[string]bool{},
	}
	byID := map[ids.WorkspaceID]wsm.Workspace{}
	for _, ws := range workspaces {
		view.byDir[canonical(ws.Dir)] = ws
		byID[ws.ID] = ws
		if ws.Closed || ws.ParentBranch == "" {
			continue
		}
		if view.parents[ws.Repo] == nil {
			view.parents[ws.Repo] = map[string]bool{}
		}
		view.parents[ws.Repo][ws.ParentBranch] = true
	}
	for _, id := range live {
		if ws, ok := byID[id]; ok {
			view.live[canonical(ws.Dir)] = true
		}
	}
	return view
}

// repoSweep is one repository's pass.
type repoSweep struct {
	r      *Reaper
	repo   wsm.Repository
	known  registryView
	now    time.Time
	report *Report

	defaultBranch string
	base          string
	baseTree      string
}

// run sweeps the repository. Every failure is recorded and counted here; none
// ends the sweep.
func (s *repoSweep) run(ctx context.Context) {
	log := s.r.deps.Log
	repoFields := dlog.Context{"repo": string(s.repo.ID), "repo_dir": s.repo.Dir}

	present, err := dirPresent(s.repo.Dir)
	if err != nil {
		s.fail(ctx, opRepo, "the repository's main worktree could not be read; it is not swept", repoFields, err)
		return
	}
	if !present {
		log.Info(opRepo, "the repository's main worktree is gone; there is nothing to sweep", repoFields)
		return
	}

	if s.defaultBranch, err = s.r.deps.Git.DefaultBranch(ctx, s.repo.Dir); err != nil {
		s.fail(ctx, opRepo, "the repository's default branch could not be resolved; it is not swept", repoFields, err)
		return
	}
	// ONE BASE FOR THE WHOLE REPOSITORY: every worktree is judged against
	// the same commit and tree, even if the default branch moves mid-sweep.
	if s.base, err = s.r.deps.Git.ResolveRef(ctx, s.repo.Dir, "refs/heads/"+s.defaultBranch); err != nil {
		s.fail(ctx, opRepo, "the default branch could not be resolved to a commit; it is not swept", repoFields, err)
		return
	}
	if s.baseTree, err = s.r.deps.Git.TreeOf(ctx, s.repo.Dir, s.base); err != nil {
		s.fail(ctx, opRepo, "the default branch's tree could not be resolved; it is not swept", repoFields, err)
		return
	}
	worktrees, err := s.r.deps.Git.ListWorktrees(ctx, s.repo.Dir)
	if err != nil {
		s.fail(ctx, opRepo, "the repository's worktrees could not be listed; it is not swept", repoFields, err)
		return
	}

	s.prune(ctx, worktrees)
	mainDir := canonical(s.repo.Dir)
	for i, wt := range worktrees {
		if ctx.Err() != nil {
			return
		}
		if i == 0 || canonical(wt.Dir) == mainDir {
			s.report.Kept[keepMain]++
			continue
		}
		s.report.Worktrees++
		s.judge(ctx, wt)
	}
	s.report.Repositories++
}

// prune retires the stale registrations, once per repository, when git
// reports any.
func (s *repoSweep) prune(ctx context.Context, worktrees []gitclient.Worktree) {
	var stale []dlog.Context
	for _, wt := range worktrees {
		if wt.Prunable && !wt.Locked {
			stale = append(stale, dlog.Context{"worktree": wt.Dir, "branch": wt.Branch, "head": wt.Head, "why": wt.PrunableReason})
		}
	}
	if len(stale) == 0 {
		return
	}
	fields := dlog.Context{"repo": string(s.repo.ID), "repo_dir": s.repo.Dir, "prunable": stale}
	if err := s.r.deps.Git.PruneWorktrees(ctx, s.repo.Dir); err != nil {
		s.fail(ctx, opPrune, "the stale worktree registrations could not be pruned", fields, err)
		return
	}
	s.report.Pruned++
	s.r.deps.Log.Info(opPrune, "pruned the registrations of worktrees whose directories are gone", fields)
}

// judge decides one linked worktree, in the order that asks git the least:
// every gate that needs no git runs first, the landed test runs last.
func (s *repoSweep) judge(ctx context.Context, wt gitclient.Worktree) {
	dir := canonical(wt.Dir)
	fields := dlog.Context{
		"repo": string(s.repo.ID), "repo_dir": s.repo.Dir,
		"worktree": wt.Dir, "branch": wt.Branch, "head": wt.Head,
	}
	ws, registered := s.known.byDir[dir]
	if registered {
		fields["workspace"] = string(ws.ID)
	}

	switch {
	case wt.Bare:
		s.keep(keepBare, fields)
		return
	case wt.Locked:
		fields["lock_reason"] = wt.LockedReason
		s.keep(keepLocked, fields)
		return
	case wt.Prunable:
		s.keep(keepPrunable, fields)
		return
	case registered && !ws.Closed:
		s.keep(keepOpenWorkspace, fields)
		return
	case s.known.live[dir]:
		s.keep(keepLiveSession, fields)
		return
	case wt.Branch != "" && s.known.parents[s.repo.ID][wt.Branch]:
		s.keep(keepParentBranch, fields)
		return
	case wt.Branch == s.defaultBranch:
		s.keep(keepDefaultBranch, fields)
		return
	case !isCommit(wt.Head):
		s.keep(keepNoCommit, fields)
		return
	}

	present, err := dirPresent(wt.Dir)
	if err != nil {
		s.fail(ctx, opJudge, "the worktree's directory could not be read; it is kept", fields, err)
		return
	}
	if !present {
		s.keep(keepMissing, fields)
		return
	}

	// ACTIVITY IS READ BEFORE ANY PROBE, so nothing this sweep runs can be
	// mistaken for somebody working in the tree.
	var record *wsm.Workspace
	if registered {
		record = &ws
	}
	last, err := lastActivity(ctx, s.r.deps.Git, s.repo.Dir, wt, record)
	if err != nil {
		s.fail(ctx, opJudge, "the worktree's last activity could not be read; it is kept", fields, err)
		return
	}
	fields["last_activity"] = last.At.Format(time.RFC3339)
	fields["last_activity_signal"] = last.Signal
	idle := s.now.Sub(last.At)
	fields["idle_for"] = idle.Round(time.Second).String()
	if idle < s.r.deps.IdleAfter {
		s.keep(keepActive, fields)
		return
	}

	clean, err := s.r.deps.Git.IsClean(ctx, wt.Dir)
	if err != nil {
		s.fail(ctx, opJudge, "the worktree's cleanliness could not be read; it is kept", fields, err)
		return
	}
	if !clean {
		s.keep(keepDirty, fields)
		return
	}

	merged, err := s.r.deps.Git.MergeTree(ctx, s.repo.Dir, s.base, wt.Head)
	if err != nil {
		s.fail(ctx, opJudge, "the worktree's merge into the default branch could not be computed; it is kept", fields, err)
		return
	}
	if merged.Conflicted {
		s.keep(keepConflicts, fields)
		return
	}
	if merged.Tree != s.baseTree {
		s.keep(keepUnlanded, fields)
		return
	}

	s.remove(ctx, wt, fields)
}

// remove retires a landed worktree and then its branch. The branch goes ONLY
// after the tree did, and only while it still points at the head that was
// judged.
func (s *repoSweep) remove(ctx context.Context, wt gitclient.Worktree, fields dlog.Context) {
	log := s.r.deps.Log
	fields["default_branch"] = s.defaultBranch
	fields["base"] = s.base
	fields["why"] = fmt.Sprintf("merging %s into %s at %s changes nothing: the merge's tree is %s's own tree %s, and the worktree is clean and idle",
		wt.Head, s.defaultBranch, s.base, s.defaultBranch, s.baseTree)

	if err := s.r.deps.Git.RemoveCleanWorktree(ctx, s.repo.Dir, wt.Dir); err != nil {
		s.report.Kept[reasonRemoveFailed]++
		s.fail(ctx, opRemove, "the landed worktree could not be removed; it and its branch are kept", fields, err)
		return
	}
	s.report.Removed = append(s.report.Removed, wt.Dir)
	log.Info(opRemove, "removed a worktree whose changes have landed on the default branch", fields)

	if wt.Branch == "" {
		return
	}
	branchFields := dlog.Context{
		"repo": string(s.repo.ID), "repo_dir": s.repo.Dir, "worktree": wt.Dir,
		"branch": wt.Branch, "head": wt.Head,
	}
	if err := s.r.deps.Git.DeleteBranchAt(ctx, s.repo.Dir, wt.Branch, wt.Head); err != nil {
		s.fail(ctx, opBranch, "the removed worktree's branch could not be deleted at the judged head; the branch is kept", branchFields, err)
		return
	}
	s.report.BranchesDeleted++
	log.Info(opBranch, "deleted the removed worktree's branch; its content is on the default branch", branchFields)
}

// keep counts and records a worktree left alone.
func (s *repoSweep) keep(reason string, fields dlog.Context) {
	s.report.Kept[reason]++
	fields["reason"] = reason
	s.r.deps.Log.Debug(opKeep, "kept a worktree", fields)
}

// fail records one failed step. The daemon's own exit cancelling a git is not
// a failure: it is recorded at INFO and counted nowhere.
func (s *repoSweep) fail(ctx context.Context, operation, message string, fields dlog.Context, err error) {
	record := dlog.Context{"cause": err.Error()}
	for k, v := range fields {
		record[k] = v
	}
	if ctx.Err() != nil || gitclient.IsCancelled(err) {
		s.r.deps.Log.Info(operation, "the step stopped with the daemon", record)
		return
	}
	s.report.Failures++
	s.r.deps.Log.Error(operation, message, record)
}

// isCommit reports whether a porcelain HEAD names a commit: an unborn branch
// is listed with the all-zero id.
func isCommit(head string) bool {
	if head == "" {
		return false
	}
	for _, r := range head {
		if r != '0' {
			return true
		}
	}
	return false
}

// canonical is a directory's comparable form: symlinks resolved where the path
// exists (macOS reaches one tree through /tmp and /private/tmp), clean where it
// does not.
func canonical(dir string) string {
	if resolved, err := filepath.EvalSymlinks(dir); err == nil {
		return filepath.Clean(resolved)
	}
	return filepath.Clean(dir)
}

// dirPresent reports whether a directory exists. A stat that cannot tell is an
// error, never read as absence.
func dirPresent(dir string) (bool, error) {
	_, err := os.Stat(dir)
	switch {
	case err == nil:
		return true, nil
	case errors.Is(err, os.ErrNotExist):
		return false, nil
	default:
		return false, fmt.Errorf("worktreereap: reading %s: %w", dir, err)
	}
}
