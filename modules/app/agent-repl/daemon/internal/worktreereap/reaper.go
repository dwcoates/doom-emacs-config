// Package worktreereap is the daemon's LANDED-WORKTREE REAPER: a low-priority
// background sweep that removes every linked worktree whose changes have
// already landed on its repository's default branch, and deletes its branch.
// It also EXPIRES the linked worktrees that never land: see expiry.go.
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
// It also removes the LEFTOVERS beside a repository's worktrees: a directory
// in its worktrees directory that is no worktree and holds nothing but log
// links (`.claude/emacs/*.log`), which a runtime re-made inside a worktree as
// it was being removed. See leftovers.go.
//
// A sweep runs on its schedule, or now on request (SweepWith, the
// ReapWorktrees rpc), where it may judge without acting (Options.DryRun) or
// skip the windows for closed workspaces' worktrees (Options.Force).
//
// See daemon/AGENTS.md "The landed-worktree reaper" for the safety gates.
package worktreereap

import (
	"context"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"time"

	"agent-repld/internal/clock"
	"agent-repld/internal/dlog"
	"agent-repld/internal/gitclient"
	"agent-repld/internal/ids"
	"agent-repld/internal/sweepguard"
	"agent-repld/internal/wsm"
)

// The records this package writes.
const (
	opSweep    = "daemon.worktreereap.sweep"
	opRepo     = "daemon.worktreereap.repo"
	opPrune    = "daemon.worktreereap.prune"
	opKeep     = "daemon.worktreereap.keep"
	opJudge    = "daemon.worktreereap.judge"
	opRemove   = "daemon.worktreereap.remove"
	opBranch   = "daemon.worktreereap.branch"
	opExpire   = "daemon.worktreereap.expire"
	opRun      = "daemon.worktreereap.run"
	opLeftover = "daemon.worktreereap.leftover"
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
	// DefaultExpireAfter is how long a worktree that never lands may exist,
	// counted over unbroken repository activity (see expiry.go), before it
	// is preserved and removed.
	DefaultExpireAfter = 7 * 24 * time.Hour
)

// ErrSweepRunning is Sweep's refusal while another sweep of this reaper is in
// flight. The sweep never runs concurrently with itself.
var ErrSweepRunning = sweepguard.ErrRunning

// ErrHeldElsewhere is SweepWith's answer when another daemon holds the
// sweep's kernel lock: nothing was swept, by this process.
var ErrHeldElsewhere = errors.New("worktreereap: another daemon is sweeping")

// Options shape one sweep. The schedule sweeps with the zero Options.
type Options struct {
	// DryRun judges every worktree and leftover exactly as a sweep does and
	// removes, prunes, preserves and deletes nothing: the report is what a
	// sweep at that instant would do.
	DryRun bool
	// Force skips the idle window and the expiry window for the worktree of a
	// CLOSED workspace: landed, it is removed at once; unlanded, it is
	// preserved and removed at once. It never touches an open workspace's
	// worktree, nor one no workspace names.
	Force bool
}

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
	RemoveWorktree(ctx context.Context, repoDir, worktreeDir string) error
	CommonDir(ctx context.Context, dir string) (string, error)
	PreserveWorktree(ctx context.Context, repoDir, worktreeDir, ref, message string) (string, error)
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
	// IdleAfter, ExpireAfter, StartDelay and Every are the windows; see the
	// defaults.
	IdleAfter   time.Duration
	ExpireAfter time.Duration
	StartDelay  time.Duration
	Every       time.Duration
	// WorktreeParent answers the directory a repository's linked worktrees
	// are made in (workspace.WorktreeParent), where its leftovers lie.
	WorktreeParent func(repoDir string) (string, error)
	// Log is the global logger: most of what the reaper judges is no
	// workspace's.
	Log dlog.Logger
}

// Reaper is the landed-worktree reaper.
type Reaper struct {
	deps Deps
	// guard serializes the sweeps, in this process and across daemons.
	guard *sweepguard.Guard
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
	case deps.WorktreeParent == nil:
		return nil, errors.New("worktreereap: WorktreeParent is required")
	case deps.IdleAfter <= 0, deps.ExpireAfter <= 0, deps.StartDelay <= 0, deps.Every <= 0:
		return nil, fmt.Errorf("worktreereap: the windows must be positive (idle after %v, expire after %v, start delay %v, every %v)",
			deps.IdleAfter, deps.ExpireAfter, deps.StartDelay, deps.Every)
	}
	return &Reaper{deps: deps, guard: sweepguard.New("worktreereap", deps.LockPath, opSweep)}, nil
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
	keepNotLeftover    = "not_only_log_links"
	reasonRemoveFailed = "remove_failed"
	reasonPreserveFail = "preserve_failed"
)

// Report is one sweep's account.
type Report struct {
	// DryRun says nothing was done: every list below is what a sweep would do.
	DryRun bool
	// Repositories is how many repositories were swept to the end.
	Repositories int
	// Worktrees is how many linked worktrees were judged.
	Worktrees int
	// Removed lists every removed worktree directory.
	Removed []string
	// Expired lists every worktree directory removed by the expiry rule.
	Expired []string
	// Forced lists every unlanded worktree of a closed workspace a forced
	// sweep preserved and removed without waiting out the expiry window.
	Forced []string
	// Leftovers lists every leftover directory removed (leftovers.go).
	Leftovers []string
	// BranchesDeleted counts the branches deleted after their removal.
	BranchesDeleted int
	// Pruned counts the repositories whose stale registrations were pruned.
	Pruned int
	// Kept counts the worktrees left alone, by reason.
	Kept map[string]int
	// Failures counts every step that failed and was recorded at ERROR.
	Failures int
}

// Sweep is the schedule's sweep: SweepWith the zero Options, answering a zero
// report without sweeping while another PROCESS holds the sweep's kernel lock
// (the guard records that at INFO).
func (r *Reaper) Sweep(ctx context.Context) (Report, error) {
	report, err := r.SweepWith(ctx, Options{})
	if errors.Is(err, ErrHeldElsewhere) {
		return Report{}, nil
	}
	return report, err
}

// SweepWith runs one sweep over every repository the registry knows, shaped
// by OPTS. It refuses with ErrSweepRunning while another sweep of this reaper
// is in flight, and with ErrHeldElsewhere while another PROCESS holds the
// sweep's kernel lock. A failure inside one repository or one worktree is
// recorded at ERROR and the sweep goes on; only a registry that cannot be read,
// or the daemon's own exit, ends it early.
func (r *Reaper) SweepWith(ctx context.Context, opts Options) (Report, error) {
	var (
		report Report
		err    error
	)
	ran, guardErr := r.guard.Do(r.deps.Log, func() { report, err = r.sweep(ctx, opts) })
	if guardErr != nil {
		return Report{}, guardErr
	}
	if !ran {
		return Report{}, ErrHeldElsewhere
	}
	return report, err
}

// sweep is one sweep, run under the guard.
func (r *Reaper) sweep(ctx context.Context, opts Options) (Report, error) {
	now := r.deps.Clock.Now()
	report := Report{Kept: map[string]int{}, DryRun: opts.DryRun}
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
		"repositories": len(repos), "idle_after": r.deps.IdleAfter.String(), "expire_after": r.deps.ExpireAfter.String(),
		"dry_run": opts.DryRun, "force": opts.Force,
	})
	for _, repo := range repos {
		if ctx.Err() != nil {
			break
		}
		(&repoSweep{r: r, opts: opts, repo: repo, known: known, workspaces: workspaces, now: now, report: &report}).run(ctx)
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
		"expired":          len(report.Expired),
		"expired_dirs":     report.Expired,
		"forced":           len(report.Forced),
		"forced_dirs":      report.Forced,
		"leftovers":        len(report.Leftovers),
		"leftover_dirs":    report.Leftovers,
		"dry_run":          report.DryRun,
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
	r          *Reaper
	opts       Options
	repo       wsm.Repository
	known      registryView
	workspaces []wsm.Workspace
	now        time.Time
	report     *Report

	defaultBranch string
	base          string
	baseTree      string
	// activeSince is when the repository's current unbroken run of activity
	// began: no worktree's expiry clock starts before it.
	activeSince time.Time
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

	// THE REPOSITORY'S ACTIVITY IS READ BEFORE THE PRUNE AND EVERY PROBE: the
	// prune deletes stale worktrees' reflogs, which are history too.
	commonDir, err := s.r.deps.Git.CommonDir(ctx, s.repo.Dir)
	if err != nil {
		s.fail(ctx, opRepo, "the repository's common dir could not be resolved; it is not swept", repoFields, err)
		return
	}
	instants, err := repoActivity(commonDir, s.repo.ID, s.workspaces)
	if err != nil {
		repoFields["common_dir"] = commonDir
		s.fail(ctx, opRepo, "the repository's activity could not be read; it is not swept", repoFields, err)
		return
	}
	s.activeSince = activeSince(instants, s.now)

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
	if ctx.Err() != nil {
		return
	}
	s.sweepLeftovers(ctx, worktrees)
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
	if s.opts.DryRun {
		s.report.Pruned++
		s.r.deps.Log.Info(opPrune, "would prune the registrations of worktrees whose directories are gone", fields)
		return
	}
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
	// A FORCED SWEEP WAITS OUT NO WINDOW for a closed workspace's tree; an
	// open one is kept below whatever was asked.
	forced := s.opts.Force && registered && ws.Closed
	if forced {
		fields["forced"] = true
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
	admin, err := s.r.deps.Git.AdminDir(ctx, wt.Dir)
	if err != nil {
		s.fail(ctx, opJudge, "the worktree's admin directory could not be resolved; it is kept", fields, err)
		return
	}
	last, err := lastActivity(ctx, s.r.deps.Git, s.repo.Dir, admin, wt, record)
	if err != nil {
		s.fail(ctx, opJudge, "the worktree's last activity could not be read; it is kept", fields, err)
		return
	}
	fields["last_activity"] = last.At.Format(time.RFC3339)
	fields["last_activity_signal"] = last.Signal
	idle := s.now.Sub(last.At)
	fields["idle_for"] = idle.Round(time.Second).String()
	if idle < s.r.deps.IdleAfter && !forced {
		s.keep(keepActive, fields)
		return
	}

	clean, err := s.r.deps.Git.IsClean(ctx, wt.Dir)
	if err != nil {
		s.fail(ctx, opJudge, "the worktree's cleanliness could not be read; it is kept", fields, err)
		return
	}
	unlandedReason := keepDirty
	if clean {
		merged, err := s.r.deps.Git.MergeTree(ctx, s.repo.Dir, s.base, wt.Head)
		if err != nil {
			s.fail(ctx, opJudge, "the worktree's merge into the default branch could not be computed; it is kept", fields, err)
			return
		}
		switch {
		case merged.Conflicted:
			unlandedReason = keepConflicts
		case merged.Tree != s.baseTree:
			unlandedReason = keepUnlanded
		default:
			s.remove(ctx, wt, fields)
			return
		}
	}

	expired, err := s.expired(admin, fields)
	if err != nil {
		s.fail(ctx, opJudge, "the worktree's birth could not be read; it is kept", fields, err)
		return
	}
	if !expired && !forced {
		s.keep(unlandedReason, fields)
		return
	}
	fields["unlanded_reason"] = unlandedReason
	s.expire(ctx, wt, admin, clean, !expired, fields)
}

// expired applies the expiry rule. The worktree's birth is read only when the
// repository's own run of activity is already long enough to expire it, since
// the clock starts at the later of the two.
func (s *repoSweep) expired(admin string, fields dlog.Context) (bool, error) {
	fields["repo_active_since"] = s.activeSince.Format(time.RFC3339)
	if s.now.Sub(s.activeSince) < s.r.deps.ExpireAfter {
		return false, nil
	}
	born, err := bornAt(admin)
	if err != nil {
		return false, err
	}
	start := s.activeSince
	if born.After(start) {
		start = born
	}
	fields["born"] = born.Format(time.RFC3339)
	fields["expiry_age"] = s.now.Sub(start).Round(time.Second).String()
	return s.now.Sub(start) >= s.r.deps.ExpireAfter, nil
}

// expire preserves an expired worktree under refs/agent-repl/reaped/ and then
// removes it. Its branch is KEPT: what the branch holds never landed. A tree
// judged clean goes through the clean door, so one that became dirty after the
// preservation is refused by git and kept; only a tree judged dirty is forced,
// and what made it dirty is in the preservation commit.
//
// FORCED is a closed workspace's tree a forced sweep takes before its window
// ran out; it is preserved and removed exactly as an expired one, and listed
// apart.
func (s *repoSweep) expire(ctx context.Context, wt gitclient.Worktree, admin string, clean, forced bool, fields dlog.Context) {
	ref := reapedRef(admin, s.now)
	fields["preserved_ref"] = ref
	fields["expire_after"] = s.r.deps.ExpireAfter.String()
	message := fmt.Sprintf("agent-repl: %s (branch %q, head %s) preserved before its removal: it existed for %s of unbroken repository activity without landing",
		wt.Dir, wt.Branch, wt.Head, s.r.deps.ExpireAfter)
	if forced {
		message = fmt.Sprintf("agent-repl: %s (branch %q, head %s) preserved before its removal: its workspace is closed and a forced sweep took it before %s ran out",
			wt.Dir, wt.Branch, wt.Head, s.r.deps.ExpireAfter)
	}
	taken := &s.report.Expired
	if forced {
		taken = &s.report.Forced
	}
	if s.opts.DryRun {
		*taken = append(*taken, wt.Dir)
		s.r.deps.Log.Info(opExpire, "would preserve and remove an unlanded worktree; its branch would be kept", fields)
		return
	}

	sha, err := s.r.deps.Git.PreserveWorktree(ctx, s.repo.Dir, wt.Dir, ref, message)
	if err != nil {
		s.report.Kept[reasonPreserveFail]++
		s.fail(ctx, opExpire, "the expired worktree could not be preserved; it is kept", fields, err)
		return
	}
	fields["preserved_sha"] = sha

	remove := s.r.deps.Git.RemoveWorktree
	if clean {
		remove = s.r.deps.Git.RemoveCleanWorktree
	}
	if err := remove(ctx, s.repo.Dir, wt.Dir); err != nil {
		s.report.Kept[reasonRemoveFailed]++
		s.fail(ctx, opExpire, "the expired worktree could not be removed; it is kept, and so is its preservation", fields, err)
		return
	}
	*taken = append(*taken, wt.Dir)
	s.r.deps.Log.Info(opExpire, "removed an unlanded worktree; its content is preserved and its branch is kept", fields)
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

	if s.opts.DryRun {
		s.report.Removed = append(s.report.Removed, wt.Dir)
		log.Info(opRemove, "would remove a worktree whose changes have landed on the default branch, and its branch", fields)
		return
	}
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
