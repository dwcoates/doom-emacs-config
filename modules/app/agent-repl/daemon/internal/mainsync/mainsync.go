// Package mainsync keeps each registered repository's main checkout at its
// upstream: when a sweep of the repositories (internal/repodivergence) finds
// a primary branch behind its upstream and not ahead, the daemon
// fast-forwards it, programmatically, with no workspace involved. Only a
// fast-forward that fails in the index or the worktree (the branch diverged,
// local changes in the way, git itself failing) is told to the repository's
// main workspace, as a deferred prompt asking it to repair the checkout
// (agentrepl/v1/main_sync.proto). A fetch that failed for want of a network
// is the sweep's to log, and is told to no workspace.
//
// NOTHING ELSE WRITES THE CHECKOUT WHILE IT IS FAST-FORWARDED, BY
// CONSTRUCTION:
//
//   - the repository's merge queue, the one writer of its main branch, holds
//     its target for the sync (merge.Orchestrator.HoldTargetForSync), which
//     it grants only while no merge is in line or running, and admits none
//     until the sync releases it;
//   - the main workspace's turn sequence is held for the sync
//     (promptqueue.Queue.Exclusively), so no turn starts during it, and a turn
//     already running defers the sync to the next sweep: HEAD never moves
//     under an agent's turn.
//
// ONE FAILURE IS TOLD ONCE: the failure last told is recorded per repository
// (wsm MainSyncTold), and the same upstream commit and cause found again, by
// the next sweep or after a restart, is logged and not told again.
package mainsync

import (
	"context"
	"errors"
	"fmt"
	"strings"
	"sync"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	conversationv1 "agentrepl/proto/conversation/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"agent-repld/internal/briefprompt"
	"agent-repld/internal/clock"
	"agent-repld/internal/dlog"
	"agent-repld/internal/gitclient"
	"agent-repld/internal/ids"
	"agent-repld/internal/intakegate"
	"agent-repld/internal/merge"
	"agent-repld/internal/prompthandler"
	"agent-repld/internal/repodivergence"
	"agent-repld/internal/wsm"
)

// The operations this package records under.
const (
	opSync = "daemon.mainsync.sync"
	opGate = "daemon.mainsync.gate"
)

// BriefRepair is the brief the repair prompt is composed from.
const BriefRepair = "main-sync-repair"

// Origin is the repair prompt's origin.
const Origin = conversationv1.PromptOrigin_PROMPT_ORIGIN_MAIN_SYNC_REPAIR

// Store is the part of the state client the sync reads and writes.
type Store interface {
	ListRepositories(ctx context.Context) ([]wsm.Repository, error)
	WorkspaceByDir(ctx context.Context, dir string) (wsm.Workspace, error)
	MainSyncTold(ctx context.Context, repo ids.RepoID) (wsm.MainSyncTold, bool, error)
	RecordMainSyncTold(ctx context.Context, repo ids.RepoID, told wsm.MainSyncTold) error
}

// Git is the part of the git leaf the sync uses.
type Git interface {
	ResolveRef(ctx context.Context, repoDir, ref string) (string, error)
	FastForward(ctx context.Context, dir, commit string) error
}

// Target holds a repository's merge target for the sync.
type Target interface {
	HoldTargetForSync(ctx context.Context, dir string) (release func(), held bool, err error)
}

// Turns holds a workspace's turn sequence for the sync.
type Turns interface {
	Exclusively(ctx context.Context, ws ids.WorkspaceID, what string, perform func(context.Context) error) error
}

// Deps are the sync's collaborators: everything but the prompt handler,
// which is handed to Run.
type Deps struct {
	Store  Store
	Git    Git
	Target Target
	Turns  Turns
	// Running reports whether a turn is running in a workspace
	// (workspace.Verbs.Status's activity).
	Running func(ctx context.Context, ws ids.WorkspaceID) (bool, error)
	// Open reopens a closed workspace, so the repair prompt reaches an agent.
	Open    func(ctx context.Context, ws ids.WorkspaceID) error
	Briefs  merge.BriefLoader
	MintKey func(prefix string) (string, error)
	// Serves reports whether this daemon serves: only the serving daemon
	// writes a checkout or tells a workspace.
	Serves func() bool
	Clock  clock.Clock
	Log    dlog.Logger
}

// Syncer is the main sync.
type Syncer struct {
	deps Deps
	gate *intakegate.Gate
	mu   sync.Mutex
	// latest is each repository's newest measure not yet acted on; a newer
	// one replaces it, since only the latest state of a branch matters.
	latest map[ids.RepoID]repodivergence.Counted
	order  []ids.RepoID
	wake   chan struct{}
}

// New builds the sync, refusing a missing collaborator.
func New(deps Deps) (*Syncer, error) {
	if deps.Store == nil || deps.Git == nil || deps.Target == nil || deps.Turns == nil || deps.Running == nil || deps.Open == nil ||
		deps.Briefs == nil || deps.MintKey == nil || deps.Serves == nil || deps.Clock == nil || deps.Log == nil {
		return nil, errors.New("mainsync: every collaborator is required")
	}
	return &Syncer{deps: deps, gate: intakegate.NewFor(deps.Serves, deps.Log, opGate, "the main checkouts' syncs"),
		latest: map[ids.RepoID]repodivergence.Counted{}, wake: make(chan struct{}, 1)}, nil
}

// Observe is the divergence sweep's sink: it takes a repository's measure
// and never blocks the sweep. Only a branch behind its upstream is work.
func (s *Syncer) Observe(repo ids.RepoID, measure repodivergence.Measure) {
	counted, ok := measure.(repodivergence.Counted)
	if !ok || counted.Behind == 0 {
		return
	}
	s.mu.Lock()
	if _, pending := s.latest[repo]; !pending {
		s.order = append(s.order, repo)
	}
	s.latest[repo] = counted
	s.mu.Unlock()
	select {
	case s.wake <- struct{}{}:
	default:
	}
}

// Run syncs every observed repository, in the order observed, until ctx
// ends.
func (s *Syncer) Run(ctx context.Context, prompts prompthandler.Submitter) {
	for {
		select {
		case <-ctx.Done():
			return
		case <-s.wake:
		}
		for _, repo := range s.take() {
			s.Sync(ctx, prompts, repo.id, repo.measure)
		}
	}
}

type observed struct {
	id      ids.RepoID
	measure repodivergence.Counted
}

func (s *Syncer) take() []observed {
	s.mu.Lock()
	defer s.mu.Unlock()
	out := make([]observed, 0, len(s.order))
	for _, id := range s.order {
		out = append(out, observed{id, s.latest[id]})
	}
	s.latest, s.order = map[ids.RepoID]repodivergence.Counted{}, nil
	return out
}

// Sync acts on one repository's measure: fast-forwards a branch that is
// behind and not ahead, and tells the main workspace a fast-forward that
// cannot be made.
func (s *Syncer) Sync(ctx context.Context, prompts prompthandler.Submitter, repoID ids.RepoID, m repodivergence.Counted) {
	fields := dlog.Context{"repo_id": string(repoID), "branch": m.Branch, "upstream": m.Upstream, "ahead": m.Ahead, "behind": m.Behind}
	if !s.gate.Admits() {
		return
	}
	repo, err := s.repository(ctx, repoID)
	if err != nil {
		s.deps.Log.Error(opSync, "could not read the repository; its main checkout is not synced this sweep", dlog.WithCause(fields, err))
		return
	}
	fields["dir"] = repo.Dir
	// The git leaf records its own failures; these record what they cost.
	local, err := s.deps.Git.ResolveRef(ctx, repo.Dir, m.Branch)
	if err != nil {
		s.deps.Log.Info(opSync, "the primary branch could not be resolved; the next sweep tries again", dlog.WithCause(fields, err))
		return
	}
	upstream, err := s.deps.Git.ResolveRef(ctx, repo.Dir, m.Upstream)
	if err != nil {
		s.deps.Log.Info(opSync, "the upstream could not be resolved; the next sweep tries again", dlog.WithCause(fields, err))
		return
	}
	failure := &agentreplv1.MainSyncFailure{Repository: &workspacev1.RepositoryRef{Id: string(repo.ID), Dir: repo.Dir},
		Branch: m.Branch, Upstream: m.Upstream, LocalSha: local, UpstreamSha: upstream}
	fields["local_sha"], fields["upstream_sha"] = local, upstream
	main, err := s.deps.Store.WorkspaceByDir(ctx, repo.Dir)
	if err != nil {
		s.deps.Log.Error(opSync, "the repository's main checkout has no workspace; it is not synced, since a failure would have no agent to tell", dlog.WithCause(fields, err))
		return
	}
	fields["workspace"] = string(main.ID)
	if m.Ahead > 0 {
		failure.Cause = &agentreplv1.MainSyncFailureCause{Cause: &agentreplv1.MainSyncFailureCause_Diverged{Diverged: &agentreplv1.MainSyncFailureDiverged{
			Ahead: uint32(m.Ahead), Behind: uint32(m.Behind)}}}
		s.tell(ctx, prompts, main, failure, fields)
		return
	}
	release, held, err := s.deps.Target.HoldTargetForSync(ctx, repo.Dir)
	switch {
	case err != nil:
		s.deps.Log.Error(opSync, "could not hold the repository's merge target; its main checkout is not synced this sweep", dlog.WithCause(fields, err))
		return
	case !held:
		s.deps.Log.Debug(opSync, "a merge of the repository is in line or running; the sync waits for the next sweep", fields)
		return
	}
	defer release()
	ffErr, deferred := s.fastForward(ctx, main, repo.Dir, m.Upstream)
	switch {
	case deferred:
		s.deps.Log.Debug(opSync, "the main workspace is running a turn; the sync waits for the next sweep", fields)
	case ffErr == nil:
		s.deps.Log.Info(opSync, "fast-forwarded the main checkout to its upstream", fields)
	default:
		failure.Cause = causeOf(ffErr, m.Upstream)
		s.tell(ctx, prompts, main, failure, fields)
	}
}

// errTurnRunning defers a sync whose main workspace is in a turn.
var errTurnRunning = errors.New("mainsync: the main workspace is running a turn")

// fastForward moves DIR to UPSTREAM while nothing else may: under the main
// workspace's held turn sequence when it is open. DEFERRED is a turn already
// running, which the sync waits out to the next sweep.
func (s *Syncer) fastForward(ctx context.Context, main wsm.Workspace, dir, upstream string) (err error, deferred bool) {
	if main.Closed {
		return s.deps.Git.FastForward(ctx, dir, upstream), false
	}
	var ffErr error
	err = s.deps.Turns.Exclusively(ctx, main.ID, "main sync", func(ctx context.Context) error {
		running, err := s.deps.Running(ctx, main.ID)
		if err != nil {
			return err
		}
		if running {
			return errTurnRunning
		}
		ffErr = s.deps.Git.FastForward(ctx, dir, upstream)
		return nil
	})
	switch {
	case errors.Is(err, errTurnRunning):
		return nil, true
	case err != nil:
		return err, false
	}
	return ffErr, false
}

// causeOf is a failed fast-forward's cause: local changes git refused to
// overwrite, with the paths it named, or git failing otherwise.
func causeOf(err error, upstream string) *agentreplv1.MainSyncFailureCause {
	var gitErr *gitclient.Error
	if !errors.As(err, &gitErr) {
		return &agentreplv1.MainSyncFailureCause{Cause: &agentreplv1.MainSyncFailureCause_GitFailed{GitFailed: &agentreplv1.MainSyncFailureGitFailed{
			Command: "git merge --ff-only " + upstream, ExitStatus: -1, Stderr: err.Error()}}}
	}
	if paths := overwrittenPaths(gitErr.Stderr); len(paths) > 0 {
		return &agentreplv1.MainSyncFailureCause{Cause: &agentreplv1.MainSyncFailureCause_LocalChanges{LocalChanges: &agentreplv1.MainSyncFailureLocalChanges{Paths: paths}}}
	}
	return &agentreplv1.MainSyncFailureCause{Cause: &agentreplv1.MainSyncFailureCause_GitFailed{GitFailed: &agentreplv1.MainSyncFailureGitFailed{
		Command: "git " + strings.Join(gitErr.Args, " "), ExitStatus: int32(gitErr.ExitCode), Stderr: gitErr.Stderr}}}
}

// overwrittenPaths reads the paths git names when it refuses to overwrite
// local changes or untracked files: the tab-indented lines after its
// "would be overwritten" heading.
func overwrittenPaths(stderr string) []string {
	var paths []string
	listing := false
	for _, line := range strings.Split(stderr, "\n") {
		switch {
		case strings.Contains(line, "would be overwritten by merge:"):
			listing = true
		case listing && strings.HasPrefix(line, "\t"):
			paths = append(paths, strings.TrimSpace(line))
		default:
			listing = false
		}
	}
	return paths
}

// kindOf names a failure's cause for the told record.
func kindOf(f *agentreplv1.MainSyncFailure) string {
	switch f.GetCause().GetCause().(type) {
	case *agentreplv1.MainSyncFailureCause_Diverged:
		return "diverged"
	case *agentreplv1.MainSyncFailureCause_LocalChanges:
		return "local_changes"
	case *agentreplv1.MainSyncFailureCause_GitFailed:
		return "git_failed"
	}
	panic(fmt.Sprintf("mainsync: cause %T has no kind", f.GetCause().GetCause()))
}

// tell asks MAIN to repair its checkout, once per upstream commit and cause,
// reopening it first when it is closed.
func (s *Syncer) tell(ctx context.Context, prompts prompthandler.Submitter, main wsm.Workspace, f *agentreplv1.MainSyncFailure, fields dlog.Context) {
	kind := kindOf(f)
	fields["cause"] = kind
	repo := ids.RepoID(f.GetRepository().GetId())
	told, found, err := s.deps.Store.MainSyncTold(ctx, repo)
	if err != nil {
		s.deps.Log.Error(opSync, "could not read the failure last told; the main workspace is not told this sweep", dlog.WithCause(fields, err))
		return
	}
	if found && told.UpstreamSHA == f.GetUpstreamSha() && told.Cause == kind {
		s.deps.Log.Info(opSync, "the main checkout still cannot be fast-forwarded, as its workspace was already told", fields)
		return
	}
	if main.Closed {
		if err := s.deps.Open(ctx, main.ID); err != nil {
			s.deps.Log.Error(opSync, "could not reopen the main workspace to tell it the failed sync; it is told on a later sweep", dlog.WithCause(fields, err))
			return
		}
	}
	turn, err := briefprompt.Composer{Briefs: s.deps.Briefs, MintKey: s.deps.MintKey}.
		Submit(ctx, prompts, main.ID, BriefRepair, briefValues(f), "main-sync-repair", Origin)
	switch briefprompt.StepOf(err) {
	case briefprompt.StepCompose:
		s.deps.Log.Error(opSync, "could not compose the repair prompt; the main workspace is told on a later sweep", dlog.WithCause(fields, err))
		return
	case briefprompt.StepMint:
		s.deps.Log.Error(opSync, "could not mint the repair prompt's idempotency key; the main workspace is told on a later sweep", dlog.WithCause(fields, err))
		return
	case briefprompt.StepSubmit:
		s.deps.Log.Error(opSync, "could not submit the repair prompt to the main workspace; it is told on a later sweep", dlog.WithCause(fields, err))
		return
	}
	fields["turn"] = string(turn)
	if err := s.deps.Store.RecordMainSyncTold(ctx, repo, wsm.MainSyncTold{UpstreamSHA: f.GetUpstreamSha(), Cause: kind, At: s.deps.Clock.Now()}); err != nil {
		s.deps.Log.Error(opSync, "told the main workspace but could not record it; the next sweep may tell it again", dlog.WithCause(fields, err))
		return
	}
	s.deps.Log.Info(opSync, "the main checkout could not be fast-forwarded; asked its workspace to repair it", fields)
}

// briefValues are the repair brief's placeholders.
func briefValues(f *agentreplv1.MainSyncFailure) map[string]string {
	var cause string
	switch c := f.GetCause().GetCause().(type) {
	case *agentreplv1.MainSyncFailureCause_Diverged:
		cause = fmt.Sprintf("`%s` has diverged from `%s`: it holds %d commit(s) the upstream lacks, and lacks %d of the upstream's.",
			f.GetBranch(), f.GetUpstream(), c.Diverged.GetAhead(), c.Diverged.GetBehind())
	case *agentreplv1.MainSyncFailureCause_LocalChanges:
		cause = "Local changes in the checkout touch files the fast-forward would change, so git refused and changed nothing:\n- `" +
			strings.Join(c.LocalChanges.GetPaths(), "`\n- `") + "`"
	case *agentreplv1.MainSyncFailureCause_GitFailed:
		cause = fmt.Sprintf("`%s` exited %d:\n~~~text\n%s\n~~~", c.GitFailed.GetCommand(), c.GitFailed.GetExitStatus(), strings.TrimSpace(c.GitFailed.GetStderr()))
	}
	return map[string]string{
		"repository":   f.GetRepository().GetDir(),
		"branch":       f.GetBranch(),
		"upstream":     f.GetUpstream(),
		"local_sha":    f.GetLocalSha(),
		"upstream_sha": f.GetUpstreamSha(),
		"cause":        cause,
	}
}

// repository finds REPO in the registry.
func (s *Syncer) repository(ctx context.Context, id ids.RepoID) (wsm.Repository, error) {
	repos, err := s.deps.Store.ListRepositories(ctx)
	if err != nil {
		return wsm.Repository{}, err
	}
	for _, r := range repos {
		if r.ID == id {
			return r, nil
		}
	}
	return wsm.Repository{}, fmt.Errorf("mainsync: repository %s is not registered", id)
}
