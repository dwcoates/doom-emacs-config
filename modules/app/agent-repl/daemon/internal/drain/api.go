// Package drain owns the shutdown schedule and the idle sweep.
//
// It never imports the prompt queue or the merge orchestrator; the three meet
// at the WSM lease and at the shim client. Its lease policy is HOLD: new
// submissions park rather than erroring. See ARCHITECTURE.md "drain".
package drain

import (
	"context"
	"time"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/notimpl"
	"claude-repld/internal/wsm"
)

// Controller is the drain and idle-sweep surface.
type Controller interface {
	// Schedule puts a drain-and-exit schedule in force, replacing any current
	// one. It is the deploy tooling's control.
	Schedule(ctx context.Context, s wsm.DrainSchedule) error
	// Cancel clears the schedule in force. Cancelling an absent one is
	// success.
	Cancel(ctx context.Context) error
	// Current reports the schedule in force, nil when none is.
	Current(ctx context.Context) (*wsm.DrainSchedule, error)
	// Sweep runs one pass of the idle sweep: every session whose last
	// engagement is older than the cutoff is HIBERNATED under a drain lease.
	// Hibernation is the sweep's directive and belongs to no other flow — the
	// relaunch engine deliberately does not use it.
	Sweep(ctx context.Context, now time.Time) ([]ids.WorkspaceID, error)
	// Run drives the sweep on its own cadence until ctx is cancelled.
	Run(ctx context.Context) error
}

// Deps are the controller's collaborators.
type Deps struct {
	// DB holds the schedule and the last-engagement facts.
	DB wsm.DB
	// IdleCutoff is how long a session may go unengaged before the sweep
	// hibernates it (the -idle-cutoff flag).
	IdleCutoff time.Duration
	// Hibernate stands one session down. Injected so drain does not own the
	// shim fleet.
	Hibernate HibernateFunc
	// Log is the controller's logger.
	Log dlog.Surfaces
}

// HibernateFunc hibernates one workspace's session under a lease the caller
// already holds.
type HibernateFunc func(ctx context.Context, ws ids.WorkspaceID) error

// New builds the controller.
func New(deps Deps) (Controller, error) {
	return nil, notimpl.Err
}
