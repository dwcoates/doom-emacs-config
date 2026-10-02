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
//
// THE ONE EXCEPTION IS `UpdateShutdownSchedule{now}`. That request states its
// own bargain in the proto — "stop accepting work, flush in-flight writes, go"
// — and buys no freeness at all, so ShutdownNow forces every session down
// before it exits. Everything graceful (the scheduled drain's `fire`, the idle
// sweep) still waits.
package drain

import (
	"context"
	"errors"
	"time"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	shimv1 "agentrepl/proto/shim/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/shimclient"
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
	// Republish re-arms and re-announces a schedule that OUTLIVED the process
	// that put it in force. The daemon topic replays only this process's own
	// latest value, so without this a client reconnecting after a bounce would
	// silently lose a shutdown still standing. It is the boot's call, made
	// once the push surface exists and before anything is served; nothing
	// scheduled is not a refusal here — the boot simply has nothing to say.
	Republish(ctx context.Context) error
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
	// shim contact the controller has for a REGISTERED session.
	Stand Stand
	// Spawns is the shim supervisor's own sweep, and the only contact the
	// controller has with a shim NOTHING has registered yet. Required: an
	// immediate shutdown without it is the leak this exists to close.
	Spawns SpawnSweep
	// StandBound is how long ONE of those shim round trips has to answer
	// before the sweep gives that workspace up and moves on. Zero takes
	// DefaultStandBound.
	StandBound time.Duration
	// Freeness answers, and waits for, a workspace's freeness. Teardown never
	// interrupts: the wait is the whole mechanism.
	Freeness Freeness
	// Reviving reports whether the prompt queue is reviving the workspace's
	// session for a held prompt. REQUIRED: a revival's bring-up serves the
	// session before its prompt is delivered, and a sweep that cannot see the
	// revival reads that session as idle and stands it down under the prompt
	// (see Sweep).
	Reviving func(ws ids.WorkspaceID) bool
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
	// PublishHost recomposes and republishes one workspace's HOST view. The
	// composer gate on that view is a function of the OCCUPANCY LEASE -- a
	// held drain or hibernation lease composes the draining arm -- and the
	// server cannot see a lease released, so the last push a host client got
	// during a stand-down was taken while the lease was still held. Without
	// this the composer stays shut after the hibernation ends, and Emacs
	// refuses the very prompt that is meant to revive the session. Nil means
	// no host surface is wired.
	PublishHost func(ws ids.WorkspaceID)
	// PublishRegistry republishes the roster's DURABLE half. The roster's arm
	// for a PARKED session is a function of the session's terminal record
	// (resolve/sidebar/status.go: "A PARKED SESSION IS IDLE, NOT BROKEN"), the
	// roster resolver publishes only on the events it is handed, and the LAST
	// event a stand-down produces -- the shim link going dead -- is handed to
	// it BEFORE this controller writes that terminal. The row resolved on that
	// event therefore reads `dead`, and nothing else republishes afterwards,
	// so a workspace the daemon parked on purpose stays painted as a fault.
	// Nil means no roster surface is wired.
	PublishRegistry func(ctx context.Context) error
	// SetParked hands the SESSION-SCOPED views -- the footer's strip and the
	// topbar's connectivity indicator -- the same park the roster reads out of
	// the terminal record. They are in-memory accumulations rather than DB
	// readers, so unlike the roster they cannot look the record up: they are
	// told, once, from the site that writes it, which is what keeps all three
	// surfaces deriving one park from one fact.
	//
	// Without it the footer's `disconnected` step answered `dead` for the link
	// the stand-down killed, and the webapp's composer gate IS that word
	// (webapp/src/main.ts: a `disconnected` status closes the composer), so
	// the parked workspace could not be handed the prompt that revives it.
	// Nil means no session-scoped view is wired.
	SetParked func(ws ids.WorkspaceID, parked bool)
	// Clock is the controller's view of time, injected so a schedule's deadline
	// and the sweep's cadence are assertable without a real one.
	Clock Clock
	// Log is the controller's logger.
	Log dlog.Surfaces
}

// Stand is how the drain controller reaches one workspace's shim: the
// pre-hibernation directive, and the graceful stand-down that follows its ack.
type Stand interface {
	// Serving reports whether this daemon holds a shim it can address for the
	// workspace RIGHT NOW: a session installed in the fleet whose client has
	// not been reaped. It is the sweep's selection predicate, and it is the
	// same answer the directive itself resolves, so a workspace whose session
	// is not up — bring-up still in flight, a close already under way, a
	// durable session row this process never adopted — is SKIPPED rather than
	// sent a directive that cannot land.
	Serving(ws ids.WorkspaceID) bool
	// Hibernate sends the pre-hibernation directive and waits for the shim's
	// answer. The REFUSAL is an answer, not an error: turn_in_flight simply
	// defers the workspace to a later pass. It answers ErrNoLiveSession when
	// the session went away between the selection above and the directive.
	Hibernate(ctx context.Context, ws ids.WorkspaceID) (*shimv1.HibernateResponse, error)
	// KillSession stands the shim down. The SWEEP never forces: force is false
	// on every call the idle sweep makes, because teardown never interrupts a
	// drain that bought the shim's freeness by waiting for it. The IMMEDIATE
	// shutdown does force — it bought nothing, and a graceful stand-down there
	// would wait on the very turn the operator asked to stop.
	KillSession(ctx context.Context, ws ids.WorkspaceID, force bool) error
}

// SpawnSweep is the shim supervisor's stand-down of every process IT started
// and still owns. The drain reaches it only from ShutdownNow.
//
// It is a separate surface from Stand because it is keyed on nothing: Stand
// addresses one workspace's REGISTERED session, and the whole point of this
// one is the spawn that has no registration to address. The supervisor is the
// only component that can answer it, because from cmd.Start until the fleet
// remembers the session it is the only component that knows the process is
// there.
type SpawnSweep interface {
	// StandDownEverySpawn force-kills every process the supervisor started and
	// still owns, and returns every failure joined together rather than
	// stopping at the first. A shim handed to a successor by a BOUNCE is not
	// in that set: the handover detached from it, which is what tells a
	// transfer from an in-flight spawn.
	StandDownEverySpawn(ctx context.Context, reason string) error
	// BeginStandDown latches the supervisor's stand-down without sweeping,
	// answering whether this call latched it. The immediate shutdown calls it
	// BEFORE it walks the registered sessions: the latch is the one signal
	// every shim client reads to tell a departure this daemon ordered from
	// one that happened to it, and the walk itself causes departures.
	BeginStandDown() bool
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

// shimTeardownWorstCase is the whole of the shim's own graceful teardown, as
// the shim states it: the teardown spends at most FIVE of its per-stage
// `WATCHER_CONCLUSION_BUDGET_MS` budgets back to back (1s each,
// agent-shim/claude/shim/src/engine/session.ts) -- the vendor's interrupt and
// per-task stops, the message loop's end, the book-head read, the tails' end
// and the bash tails -- and that teardown runs INSIDE the `KillSession` rpc the
// stand bound below covers. It was FOUR until the vendor's half was bounded
// (2026-10-02): unbounded, a stuck vendor kept a forced KillSession from ever
// answering.
const shimTeardownWorstCase = 5 * time.Second

// standBoundMargin is what separates the stand bound from the sum of the two
// promises nested inside it. MEASURED, the whole daemon-side stop takes 9ms p50
// and 15ms max across 104 e2e runs, so this is not headroom anything healthy
// consumes -- it exists so that a shim spending its own last resort in full,
// and a kill spending its grace in full, still both land INSIDE this bound
// rather than exactly on it.
const standBoundMargin = 500 * time.Millisecond

// DefaultStandBound is how long the idle sweep gives ONE shim round trip --
// the Hibernate directive, or the graceful KillSession that follows its ack --
// before it gives that workspace up for this pass.
//
// THE SWEEP RUNS ON THE DRAIN LOOP'S OWN GOROUTINE, so an unbounded call there
// is not one wedged workspace, it is the whole controller: no later pass, no
// other workspace, and no standing schedule ever fires again.
//
// IT IS A SUM OF WHAT IT CONTAINS, not a round number chosen alongside them.
// The graceful `KillSession(..., false)` this bounds is two stops in sequence,
// and both are inside it:
//
//   - the shim's own teardown, inside the rpc: shimTeardownWorstCase, 5s;
//   - the process stop that follows it: shimclient.GracefulKillBound, the
//     SIGTERM grace (1.25s) plus the SIGKILL and the reap (250ms);
//   - standBoundMargin, 500ms, so both land inside this rather than on it.
//
// IT USED TO BE 5s WHILE THE KILL GRACE WAS ALSO 5s, set independently, and
// that is a bound that can never observe what it contains: the daemon gave up
// on the workspace at the exact moment the shim would have been SIGKILLed and
// reported a shim it was still stopping as leaked. Two equal numbers are two
// different promises colliding, never a nesting.
//
// `now` FORCES, so a forced Kill skips the SIGTERM wait and pays only the
// escalation half of GracefulKillBound. The graceful path (the idle sweep's
// `KillSession(..., false)`) is the one that pays the grace, and it is the one
// this is sized for.
const DefaultStandBound = shimTeardownWorstCase + shimclient.GracefulKillBound + standBoundMargin

// ExitFunc performs the daemon's orderly exit: flush the in-flight writes and
// go. It returns only if the exit could not be started.
type ExitFunc func(ctx context.Context) error

// ErrNothingScheduled is Cancel's refusal when no schedule is in force. It is
// UpdateShutdownScheduleError.nothing_scheduled.
var ErrNothingScheduled = errors.New("drain: nothing is scheduled to cancel")

// ErrNoLiveSession is the stand's answer when the workspace has no shim this
// daemon can address. It is a STATE, not a fault: nothing is up to stand down,
// so the sweep defers the workspace at debug rather than reporting a failure
// against a session that does not exist.
//
// The sweep skips such a workspace before it ever sends the directive
// (Stand.Serving above), so this arm is the RACE — a session that went away
// between the selection and the call. It is typed rather than matched on its
// text because the sweep's other directive failures, a wedged shim and a dead
// transport among them, must keep reaching the ERROR arm.
var ErrNoLiveSession = errors.New("the workspace has no live session")

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
	if deps.Spawns == nil {
		return nil, errors.New("drain: a spawn sweep is required")
	}
	if deps.Freeness == nil {
		return nil, errors.New("drain: a freeness answer is required")
	}
	if deps.Reviving == nil {
		return nil, errors.New("drain: a revival answer is required")
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
	if deps.StandBound <= 0 {
		deps.StandBound = DefaultStandBound
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
