// Package storagesweep is the daemon's STORAGE SWEEP: a low-priority
// background sweep that deletes agent-repl's own on-disk data once nothing
// will read it again, run once a day and on demand (agentrepl.v1.PruneStorage).
//
// It owns two kinds of data:
//
//   - each workspace's shim state directory (<state root>/shim/<key>, see
//     shim.go), judged by the workspace's close record in the registry;
//   - directories of dated entries no record points at (AgedDir): the
//     profiling sessions and the test runner's reuse records, judged by age.
//
// Worktrees are the landed-worktree reaper's (internal/worktreereap) and logs
// are their writers' own retention's; neither is touched here.
//
// A DRY RUN judges exactly as a sweep does and deletes nothing, so what it
// reports is what a sweep at that instant would delete.
package storagesweep

import (
	"context"
	"errors"
	"fmt"
	"time"

	"agent-repld/internal/clock"
	"agent-repld/internal/dlog"
	"agent-repld/internal/sweepguard"
	"agent-repld/internal/wsm"
)

// The records this package writes.
const (
	opSweep = "daemon.storagesweep.sweep"
	opShim  = "daemon.storagesweep.shim"
	opAged  = "daemon.storagesweep.aged"
	opRun   = "daemon.storagesweep.run"
)

// Production windows (owner ruling 2026-10-25).
const (
	// DefaultLandedAfter is how long a workspace's shim state outlives the
	// merge that landed and closed it.
	DefaultLandedAfter = 3 * 24 * time.Hour
	// DefaultClosedAfter is how long any other closed workspace's
	// transcript backups are kept.
	DefaultClosedAfter = 30 * 24 * time.Hour
	// DefaultAgedAfter is how long an entry of an AgedDir is kept.
	DefaultAgedAfter = 30 * 24 * time.Hour
	// DefaultStartDelay is how long after the daemon starts the first sweep
	// runs: after the boot's adoptions and the editor's first opens settle.
	DefaultStartDelay = 10 * time.Minute
	// DefaultEvery is the cadence after the first sweep.
	DefaultEvery = 24 * time.Hour
)

// ErrSweepRunning is Sweep's refusal while another sweep is in flight.
var ErrSweepRunning = sweepguard.ErrRunning

// ErrHeldElsewhere is Sweep's answer when another daemon holds the sweep's
// kernel lock: nothing was swept, and nothing is wrong.
var ErrHeldElsewhere = errors.New("storagesweep: another daemon is sweeping")

// Registry is the part of the state client the sweep reads.
type Registry interface {
	ListWorkspaces(ctx context.Context) ([]wsm.Workspace, error)
}

// AgedDir is a directory whose entries are deleted once older than After.
type AgedDir struct {
	// Name names the directory in records and reports.
	Name string
	// Dir is the directory. A missing one holds nothing to sweep.
	Dir string
	// After is the age past which an entry is deleted.
	After time.Duration
	// Keep answers the entries kept whatever their age (an open profiling
	// session); nil keeps none. An error keeps the whole directory.
	Keep func() (map[string]bool, error)
}

// Deps are the sweep's collaborators and windows.
type Deps struct {
	Registry Registry
	Clock    clock.Clock
	// LockPath is the kernel lock one sweep holds for its whole run.
	LockPath string
	// ShimDir is <state root>/shim.
	ShimDir string
	// Aged are the directories swept by age.
	Aged []AgedDir
	// LandedAfter, ClosedAfter, StartDelay and Every are the windows; see the
	// defaults.
	LandedAfter time.Duration
	ClosedAfter time.Duration
	StartDelay  time.Duration
	Every       time.Duration
	Log         dlog.Logger
}

// Sweeper is the storage sweep.
type Sweeper struct {
	deps  Deps
	guard *sweepguard.Guard
}

// New builds the sweep, refusing a missing collaborator or a non-positive
// window rather than running on a zero value.
func New(deps Deps) (*Sweeper, error) {
	switch {
	case deps.Registry == nil:
		return nil, errors.New("storagesweep: Registry is required")
	case deps.Clock == nil:
		return nil, errors.New("storagesweep: Clock is required")
	case deps.LockPath == "":
		return nil, errors.New("storagesweep: LockPath is required")
	case deps.ShimDir == "":
		return nil, errors.New("storagesweep: ShimDir is required")
	case deps.Log == nil:
		return nil, errors.New("storagesweep: Log is required")
	case deps.LandedAfter <= 0, deps.ClosedAfter <= 0, deps.StartDelay <= 0, deps.Every <= 0:
		return nil, fmt.Errorf("storagesweep: the windows must be positive (landed after %v, closed after %v, start delay %v, every %v)",
			deps.LandedAfter, deps.ClosedAfter, deps.StartDelay, deps.Every)
	}
	for _, a := range deps.Aged {
		if a.Name == "" || a.Dir == "" || a.After <= 0 {
			return nil, fmt.Errorf("storagesweep: an aged directory needs a name, a directory and a positive age (%+v)", a)
		}
	}
	return &Sweeper{deps: deps, guard: sweepguard.New("storagesweep", deps.LockPath, opSweep)}, nil
}

// Removal is one entry a sweep deleted, or a dry run would have.
type Removal struct {
	// Path is what was deleted: a directory or file.
	Path string
	// Reason is the rule that deleted it.
	Reason Reason
	// Bytes is what it held.
	Bytes int64
}

// Report is one sweep's account.
type Report struct {
	// DryRun reports that nothing was deleted.
	DryRun bool
	// Removed lists every deletion, in the order made.
	Removed []Removal
	// Kept counts the entries left alone, by reason.
	Kept map[KeepReason]int
	// Failures counts every step that failed and was recorded at ERROR.
	Failures int
}

// Bytes is what every removal held together.
func (r Report) Bytes() int64 {
	var total int64
	for _, rm := range r.Removed {
		total += rm.Bytes
	}
	return total
}

// Sweep runs one sweep, or with dryRun judges one and deletes nothing. It
// refuses with ErrSweepRunning while another sweep is in flight, and with
// ErrHeldElsewhere while another daemon holds the sweep's kernel lock. A failure on one entry is recorded at ERROR and the sweep goes
// on; only a registry that cannot be read ends the shim rule early.
func (s *Sweeper) Sweep(ctx context.Context, dryRun bool) (Report, error) {
	report := Report{DryRun: dryRun, Kept: map[KeepReason]int{}}
	var err error
	ran, guardErr := s.guard.Do(s.deps.Log, func() { err = s.sweep(ctx, &report) })
	if guardErr != nil {
		return Report{}, guardErr
	}
	if !ran {
		return Report{}, ErrHeldElsewhere
	}
	return report, err
}

// sweep is one sweep, run under the guard.
func (s *Sweeper) sweep(ctx context.Context, report *Report) error {
	now := s.deps.Clock.Now()
	s.deps.Log.Info(opSweep, "sweeping agent-repl's stored data", dlog.Context{
		"dry_run": report.DryRun, "landed_after": s.deps.LandedAfter.String(), "closed_after": s.deps.ClosedAfter.String(),
	})
	err := s.sweepShims(ctx, now, report)
	for _, a := range s.deps.Aged {
		if ctx.Err() != nil {
			break
		}
		s.sweepAged(a, now, report)
	}
	if ctx.Err() != nil {
		s.deps.Log.Info(opSweep, "the sweep stopped with the daemon", summary(*report))
		return ctx.Err()
	}
	s.deps.Log.Info(opSweep, "the sweep finished", summary(*report))
	return err
}

// summary is a report's record context.
func summary(r Report) dlog.Context {
	ctx := dlog.Context{"dry_run": r.DryRun, "removed": len(r.Removed), "bytes": r.Bytes(), "failures": r.Failures}
	for reason, n := range r.Kept {
		ctx["kept_"+string(reason)] = n
	}
	return ctx
}

// Run drives the schedule for the daemon's whole serving lifetime: one sweep
// StartDelay after the start, then one every Every. A sweep's failures are
// recorded inside it and never end the loop.
func (s *Sweeper) Run(ctx context.Context) error {
	s.deps.Log.Debug(opRun, "the storage sweep is scheduled", dlog.Context{
		"start_delay": s.deps.StartDelay.String(), "every": s.deps.Every.String(),
	})
	err := clock.Repeat(ctx, s.deps.Clock, s.deps.StartDelay, s.deps.Every, func(ctx context.Context) {
		switch _, err := s.Sweep(ctx, false); {
		case errors.Is(err, ErrSweepRunning):
			s.deps.Log.Debug(opRun, "the scheduled sweep found one already running", nil)
		case errors.Is(err, ErrHeldElsewhere):
			s.deps.Log.Debug(opRun, "the scheduled sweep found another daemon sweeping", nil)
		}
	})
	s.deps.Log.Debug(opRun, "the storage sweep stopped with its context", nil)
	return err
}
