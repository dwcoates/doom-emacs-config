// Package drain owns the shutdown schedule and the idle sweep.
//
// It never imports the prompt queue or the merge orchestrator; the three meet
// at the WSM lease and at the shim client. Its lease policy is HOLD: new
// submissions park rather than erroring. See ARCHITECTURE.md "drain".
//
// TEARDOWN NEVER INTERRUPTS THE VENDOR (ruled 2026-08-28). Graceful shutdown
// means WAITING for freeness; there is no interrupt-before-disconnect step,
// and repeated refusals under the drain lease are logged RATE-LIMITED with
// exact suppressed and total counts rather than flooding the log.
package drain

import (
	"context"
	"errors"
	"time"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	shimv1 "agentrepl/proto/shim/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/wsm"
)

// Controller is the drain and idle-sweep surface.
type Controller interface {
	// Schedule puts a drain-and-exit schedule in force, replacing any current
	// one, and publishes drain_scheduled to every WatchDaemon subscriber. It is
	// the deploy tooling's control.
	Schedule(ctx context.Context, s wsm.DrainSchedule) error
	// Cancel clears the schedule in force and publishes drain_cancelled. It
	// REFUSES when nothing is scheduled: the caller asked to cancel something,
	// and there was nothing to cancel.
	Cancel(ctx context.Context) error
	// Current reports the schedule in force, nil when none is.
	Current(ctx context.Context) (*wsm.DrainSchedule, error)
	// ShutdownNow announces an immediate shutdown and exits once the in-flight
	// writes are done. It is UpdateShutdownSchedule{now}.
	ShutdownNow(ctx context.Context, reason *agentreplv1.DrainReason) error
	// NoteRefusal records that one submission was refused or held under the
	// drain lease. The record is RATE-LIMITED: one WARN per window carrying the
	// suppressed count for the window just closed and the running total.
	NoteRefusal(ws ids.WorkspaceID)
	// Sweep runs one pass of the idle sweep: every session whose last
	// engagement is older than the cutoff is HIBERNATED under a drain lease.
	// Hibernation is the sweep's directive and belongs to no other flow — the
	// relaunch engine deliberately does not use it.
	Sweep(ctx context.Context, now time.Time) ([]ids.WorkspaceID, error)
	// Run drives the sweep on its own cadence, and fires the standing schedule
	// when its deadline passes, until ctx is cancelled.
	Run(ctx context.Context) error
}

// Deps are the controller's collaborators.
type Deps struct {
	// DB holds the schedule and the last-engagement facts.
	DB wsm.DB
	// IdleCutoff is how long a session may go unengaged before the sweep
	// hibernates it (the --idle-cutoff flag). AGENT_REPL_HIBERNATE_IDLE_CUTOFF_MS
	// BEATS it; New applies that override itself so no wiring can forget to.
	IdleCutoff time.Duration
	// SweepEvery is the idle sweep's cadence under Run.
	SweepEvery time.Duration
	// RefusalWindow is how long one refusal WARN suppresses its successors.
	RefusalWindow time.Duration
	// Stand is how the controller reaches a workspace's shim. It is the only
	// shim contact the controller has.
	Stand Stand
	// Freeness answers, and waits for, a workspace's freeness. Teardown never
	// interrupts: the wait is the whole mechanism.
	Freeness Freeness
	// Announcer publishes the WatchDaemon pushes: the standing drain schedule,
	// its cancellation, and the shutdown announcement.
	Announcer Announcer
	// Exit performs the daemon's orderly exit once the drain is quiet.
	Exit ExitFunc
	// LeaseChanged tells the prompt queue that a workspace's lease set changed,
	// so the holds taken against the departed lease are re-evaluated. Without
	// it a prompt held during a hibernation stays held forever: the release
	// changes a row the queue is not watching. Nil means nothing is told.
	LeaseChanged func(ws ids.WorkspaceID)
	// Clock is the controller's view of time, injected so a schedule's deadline
	// and the sweep's cadence are assertable without a real one.
	Clock Clock
	// Log is the controller's logger.
	Log dlog.Surfaces
}

// Stand is how the drain controller reaches one workspace's shim: the
// pre-hibernation directive, and the graceful stand-down that follows its ack.
type Stand interface {
	// Hibernate sends the pre-hibernation directive and waits for the shim's
	// answer. The REFUSAL is an answer, not an error: turn_in_flight simply
	// defers the workspace to a later pass.
	Hibernate(ctx context.Context, ws ids.WorkspaceID) (*shimv1.HibernateResponse, error)
	// KillSession stands the shim down. The sweep NEVER forces: force is false
	// on every call the drain makes, because teardown never interrupts.
	KillSession(ctx context.Context, ws ids.WorkspaceID, force bool) error
}

// Freeness answers a workspace's freeness and lets a caller WAIT for it
// without polling. It is the sessionwatcher's answer, injected because drain
// sits beside the watcher rather than above it.
type Freeness interface {
	// Free reports freeness right now: no turn in flight and no live detached
	// work. A workspace with no live session is free.
	Free(ws ids.WorkspaceID) bool
	// AwaitFree blocks until the workspace is free, answering nil, or until ctx
	// ends, answering ctx.Err(). It NEVER interrupts anything to get there.
	AwaitFree(ctx context.Context, ws ids.WorkspaceID) error
}

// Announcer publishes this daemon's WatchDaemon pushes. Every client — Emacs
// and every webview — draws its standing drain banner from them.
type Announcer interface {
	// DrainScheduled publishes the standing schedule.
	DrainScheduled(push *agentreplv1.DaemonDrainScheduled)
	// DrainCancelled publishes the schedule's cancellation.
	DrainCancelled(push *agentreplv1.DaemonDrainCancelled)
	// ShutdownAnnounced publishes the stand-down announcement.
	ShutdownAnnounced(push *agentreplv1.DaemonShutdownAnnounced)
}

// ExitFunc performs the daemon's orderly exit: flush the in-flight writes and
// go. It returns only if the exit could not be started.
type ExitFunc func(ctx context.Context) error

// ErrNothingScheduled is Cancel's refusal when no schedule is in force. It is
// UpdateShutdownScheduleError.nothing_scheduled.
var ErrNothingScheduled = errors.New("drain: nothing is scheduled to cancel")

// New builds the controller. It applies the
// AGENT_REPL_HIBERNATE_IDLE_CUTOFF_MS override to the supplied cutoff, so the
// test knob wins wherever the controller is wired.
func New(deps Deps) (Controller, error) {
	if deps.DB == nil {
		return nil, errors.New("drain: a state client is required")
	}
	if deps.Stand == nil {
		return nil, errors.New("drain: a shim stand is required")
	}
	if deps.Freeness == nil {
		return nil, errors.New("drain: a freeness answer is required")
	}
	if deps.Announcer == nil {
		return nil, errors.New("drain: an announcer is required")
	}
	if deps.Exit == nil {
		return nil, errors.New("drain: an exit is required")
	}
	if deps.Log == nil {
		return nil, errors.New("drain: log surfaces are required")
	}
	if deps.Clock == nil {
		deps.Clock = SystemClock{}
	}
	if deps.SweepEvery <= 0 {
		deps.SweepEvery = DefaultSweepEvery
	}
	if deps.RefusalWindow <= 0 {
		deps.RefusalWindow = DefaultRefusalWindow
	}
	cutoff, err := ResolveIdleCutoff(deps.IdleCutoff)
	if err != nil {
		return nil, err
	}
	deps.IdleCutoff = cutoff
	// A SWEEP THAT RUNS LESS OFTEN THAN THE CUTOFF CANNOT HONOR IT. The cadence
	// is the resolution at which idleness is noticed, so a cutoff shorter than
	// the cadence would be observed a whole cadence late — every time.
	if deps.IdleCutoff > 0 && deps.SweepEvery > deps.IdleCutoff {
		deps.SweepEvery = deps.IdleCutoff
	}
	c := &controller{deps: deps, log: deps.Log.Global(), rearm: make(chan struct{}, 1)}
	c.log.Debug(opNew, "the drain controller is up", dlog.Context{
		"idle_cutoff":    deps.IdleCutoff.String(),
		"sweep_every":    deps.SweepEvery.String(),
		"refusal_window": deps.RefusalWindow.String(),
	})
	return c, nil
}
