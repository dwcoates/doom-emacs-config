// Package repodivergence measures, in the background, how far each registered
// repository's primary branch stands from the remote branch it tracks: the
// ahead/behind count the sidebar draws over a repository section's label
// (frontend.v1.RosterRepoDivergence).
//
// ONE SWEEP AT STARTUP, THEN ONE EVERY FIVE MINUTES. A sweep walks every
// registered repository in turn: it resolves the primary branch, probes its
// upstream, fetches the upstream's remote (a count against a stale
// remote-tracking ref would never move "behind"), and counts the two against
// each other. Each repository's outcome is handed to the sink the moment it is
// known, so one slow remote never holds back the others' hovers.
package repodivergence

import (
	"context"
	"errors"
	"fmt"
	"time"

	"claude-repld/internal/clock"
	"claude-repld/internal/dlog"
	"claude-repld/internal/gitclient"
	"claude-repld/internal/ids"
	"claude-repld/internal/wsm"
)

// DefaultEvery is the cadence between sweeps.
const DefaultEvery = 5 * time.Minute

// DefaultTimeout bounds one repository's measurement. A fetch from a remote
// that never answers would otherwise stall every later repository's sweep.
const DefaultTimeout = 2 * time.Minute

// The operations this package records under.
const (
	opRun     = "daemon.repodivergence.run"
	opSweep   = "daemon.repodivergence.sweep"
	opMeasure = "daemon.repodivergence.measure"
)

// Measure is one repository's outcome: exactly one of Counted, NoUpstream and
// Failed. A repository with no Measure yet is pending.
type Measure interface{ isMeasure() }

// Counted is the primary branch counted against its upstream after a fetch.
type Counted struct {
	// Branch is the primary branch's short name.
	Branch string
	// Upstream is the upstream's short name, e.g. "origin/master".
	Upstream string
	// Ahead is the commits on Branch that Upstream lacks.
	Ahead int
	// Behind is the commits on Upstream that Branch lacks.
	Behind int
}

// NoUpstream is a primary branch that tracks no remote branch.
type NoUpstream struct {
	// Branch is the primary branch's short name.
	Branch string
}

// Failed is a measurement that could not be made.
type Failed struct {
	// Reason is what failed, in words fit for the hover.
	Reason string
}

func (Counted) isMeasure()    {}
func (NoUpstream) isMeasure() {}
func (Failed) isMeasure()     {}

// Git is the part of the git leaf a measurement uses.
type Git interface {
	DefaultBranch(ctx context.Context, repoDir string) (string, error)
	Upstream(ctx context.Context, repoDir, branch string) (gitclient.Upstream, bool, error)
	Fetch(ctx context.Context, dir, remote string) error
	AheadBehind(ctx context.Context, dir, local, upstream string) (int, int, error)
}

// Registry is the part of the state client a sweep reads.
type Registry interface {
	ListRepositories(ctx context.Context) ([]wsm.Repository, error)
}

// Sink is told each repository's outcome as soon as it is known.
type Sink func(repo ids.RepoID, measure Measure)

// Deps are the measurer's collaborators and windows.
type Deps struct {
	// Git is the git leaf.
	Git Git
	// Registry is the state client.
	Registry Registry
	// Sink is told every outcome (the sidebar resolver's SetRepoDivergence).
	Sink Sink
	// Clock is the schedule's view of time.
	Clock clock.Clock
	// Every is the cadence between sweeps; Timeout bounds one repository.
	Every   time.Duration
	Timeout time.Duration
	// Log is the global logger: a repository is no workspace's.
	Log dlog.Logger
}

// Measurer is the background ahead/behind measurer.
type Measurer struct {
	deps Deps
}

// New builds the measurer, refusing a missing collaborator or a non-positive
// window rather than running on a zero value.
func New(deps Deps) (*Measurer, error) {
	switch {
	case deps.Git == nil:
		return nil, errors.New("repodivergence: Git is required")
	case deps.Registry == nil:
		return nil, errors.New("repodivergence: Registry is required")
	case deps.Sink == nil:
		return nil, errors.New("repodivergence: Sink is required")
	case deps.Clock == nil:
		return nil, errors.New("repodivergence: Clock is required")
	case deps.Log == nil:
		return nil, errors.New("repodivergence: Log is required")
	case deps.Every <= 0, deps.Timeout <= 0:
		return nil, fmt.Errorf("repodivergence: the windows must be positive (every %v, timeout %v)", deps.Every, deps.Timeout)
	}
	return &Measurer{deps: deps}, nil
}

// Run drives the schedule for the daemon's whole serving lifetime: one sweep
// at once, then one every Every. Sweeps run one after the other on this
// goroutine, so the schedule never overlaps itself, and a sweep's failures
// never end the loop.
func (m *Measurer) Run(ctx context.Context) error {
	m.deps.Log.Debug(opRun, "the repository divergence measurer is scheduled", dlog.Context{"every": m.deps.Every.String()})
	m.Sweep(ctx)
	err := clock.Repeat(ctx, m.deps.Clock, m.deps.Every, m.deps.Every, m.Sweep)
	m.deps.Log.Debug(opRun, "the repository divergence measurer stopped with its context", nil)
	return err
}

// Sweep measures every registered repository once, handing each outcome to the
// sink. A context that ends mid-sweep stops it without telling the sink
// anything for the repository it interrupted: the daemon stopping is no fact
// about that repository.
func (m *Measurer) Sweep(ctx context.Context) {
	repos, err := m.deps.Registry.ListRepositories(ctx)
	if err != nil {
		// The state client recorded the refusal; this is what it cost.
		m.deps.Log.Info(opSweep, "the repositories could not be read; nothing is measured this sweep", dlog.Context{"cause": err.Error()})
		return
	}
	for _, repo := range repos {
		measure, stopped := m.measure(ctx, repo)
		if stopped {
			m.deps.Log.Debug(opSweep, "the sweep stopped with its context", dlog.Context{"repo_id": string(repo.ID)})
			return
		}
		m.deps.Sink(repo.ID, measure)
	}
	m.deps.Log.Debug(opSweep, "measured every repository", dlog.Context{"repositories": len(repos)})
}

// measure takes one repository's measurement. STOPPED reports that the
// caller's context ended, in which case there is no measure.
func (m *Measurer) measure(parent context.Context, repo wsm.Repository) (measure Measure, stopped bool) {
	ctx, cancel := context.WithTimeout(parent, m.deps.Timeout)
	defer cancel()
	fields := dlog.Context{"repo_id": string(repo.ID), "dir": repo.Dir}

	// fail turns a step's error into the hover's reason. The git leaf has
	// already recorded the failure itself (its ownership point), so this
	// records the outcome, not the error a second time.
	fail := func(step string, err error) (Measure, bool) {
		if parent.Err() != nil {
			return nil, true
		}
		reason := fmt.Sprintf("%s failed: %v", step, err)
		if errors.Is(err, context.DeadlineExceeded) {
			reason = fmt.Sprintf("%s did not finish within %v", step, m.deps.Timeout)
		}
		fields["step"] = step
		fields["cause"] = err.Error()
		m.deps.Log.Info(opMeasure, "the repository's divergence could not be measured", fields)
		return Failed{Reason: reason}, false
	}

	branch, err := m.deps.Git.DefaultBranch(ctx, repo.Dir)
	if err != nil {
		return fail("resolving the primary branch", err)
	}
	fields["branch"] = branch
	upstream, tracked, err := m.deps.Git.Upstream(ctx, repo.Dir, branch)
	if err != nil {
		return fail("reading "+branch+"'s upstream", err)
	}
	if !tracked {
		m.deps.Log.Debug(opMeasure, "the primary branch tracks no upstream", fields)
		return NoUpstream{Branch: branch}, false
	}
	fields["upstream"] = upstream.Ref
	if err := m.deps.Git.Fetch(ctx, repo.Dir, upstream.Remote); err != nil {
		return fail("fetching "+upstream.Remote, err)
	}
	ahead, behind, err := m.deps.Git.AheadBehind(ctx, repo.Dir, branch, upstream.Ref)
	if err != nil {
		return fail("counting "+branch+" against "+upstream.Ref, err)
	}
	fields["ahead"] = ahead
	fields["behind"] = behind
	m.deps.Log.Debug(opMeasure, "measured the repository's divergence", fields)
	return Counted{Branch: branch, Upstream: upstream.Ref, Ahead: ahead, Behind: behind}, false
}
