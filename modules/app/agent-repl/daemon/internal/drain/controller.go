package drain

import (
	"context"
	"errors"
	"fmt"
	"os"
	"strconv"
	"sync"
	"time"

	agentreplv1 "agentrepl/proto/agentrepl/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/ids"
	"claude-repld/internal/wsm"
)

// The controller's operation names. Every logical branch records under one of
// them, per the logging contract.
const (
	opNew       = "daemon.drain.new"
	opSchedule  = "daemon.drain.schedule"
	opCancel    = "daemon.drain.cancel"
	opCurrent   = "daemon.drain.current"
	opRepublish = "daemon.drain.republish"
	opNow       = "daemon.drain.shutdown_now"
	opFire      = "daemon.drain.fire"
	opRefusal   = "daemon.drain.refusal"
	opSweep     = "daemon.drain.sweep"
	opRun       = "daemon.drain.run"
)

// IdleCutoffEnv compresses the idle cutoff for tests, in milliseconds. It BEATS
// the --idle-cutoff flag: a test that sets it must not also have to know how
// the daemon was launched.
const IdleCutoffEnv = "AGENT_REPL_HIBERNATE_IDLE_CUTOFF_MS"

// DefaultIdleCutoff is the cutoff when neither the flag nor the environment
// names one.
const DefaultIdleCutoff = 12 * time.Hour

// DefaultSweepEvery is the idle sweep's cadence under Run.
const DefaultSweepEvery = 5 * time.Minute

// DefaultRefusalWindow is how long one refusal WARN suppresses its successors.
const DefaultRefusalWindow = time.Minute

// ResolveIdleCutoff answers the effective idle cutoff: IdleCutoffEnv when it is
// set, else the flag's value, else DefaultIdleCutoff. A malformed environment
// value is an ERROR — a test knob that silently did nothing would make the
// suite it was set for lie.
func ResolveIdleCutoff(flagValue time.Duration) (time.Duration, error) {
	if raw := os.Getenv(IdleCutoffEnv); raw != "" {
		ms, err := strconv.ParseInt(raw, 10, 64)
		if err != nil {
			return 0, fmt.Errorf("drain: %s=%q is not a whole number of milliseconds: %w", IdleCutoffEnv, raw, err)
		}
		if ms <= 0 {
			return 0, fmt.Errorf("drain: %s=%q is not a positive duration", IdleCutoffEnv, raw)
		}
		return time.Duration(ms) * time.Millisecond, nil
	}
	if flagValue > 0 {
		return flagValue, nil
	}
	return DefaultIdleCutoff, nil
}

// controller is the Controller implementation.
type controller struct {
	deps Deps
	log  dlog.Logger

	// rearm wakes Run when the standing schedule changes, so a schedule armed
	// between two sweeps is noticed at its own deadline rather than at the
	// next tick.
	rearm chan struct{}

	mu sync.Mutex
	// fired records that the schedule already fired, so a second deadline pass
	// does not announce twice.
	fired bool
	// refusals is the rate-limited refusal record's state.
	refusals refusalWindow
	// scheduled are the drain holds taken when the schedule was put in force,
	// by workspace.
	scheduled map[ids.WorkspaceID]ids.LeaseID
	// retries is the idle sweep's backoff for workspaces whose hibernation
	// FAILED, by workspace. See hibernateRetryDelay.
	retries map[ids.WorkspaceID]hibernateRetry
}

// Schedule puts a schedule in force and announces it. The persisted row and
// the push carry the SAME reason, because the push is rendered from the row
// that was just written rather than from the caller's argument.
func (c *controller) Schedule(ctx context.Context, s wsm.DrainSchedule) error {
	fields := dlog.Context{"deadline": s.Deadline, "set_at": s.SetAt}
	if s.SetAt.IsZero() {
		s.SetAt = c.deps.Clock.Now()
		fields["set_at"] = s.SetAt
	}
	reason, err := DecodeReason(s.Reason)
	if err != nil {
		c.log.Error(opSchedule, "refused a schedule whose reason will not decode", withCause(fields, err))
		return err
	}
	if err := c.deps.DB.PutDrainSchedule(ctx, s); err != nil {
		c.log.Error(opSchedule, "could not put the drain schedule in force", withCause(fields, err))
		return fmt.Errorf("drain: schedule: %w", err)
	}
	c.mu.Lock()
	c.fired = false
	c.mu.Unlock()

	fields["schedule"] = ScheduleID(s)
	c.deps.Announcer.DrainScheduled(&agentreplv1.DaemonDrainScheduled{
		AtMs:   milliseconds(s.Deadline),
		Reason: reason,
	})
	// THE HOLD BEGINS WITH THE SCHEDULE, not with its deadline. A prompt
	// submitted while a shutdown stands would be delivered into a session the
	// daemon is about to stand down; held instead, it survives the restart and
	// the tray tells the user WHICH shutdown it is waiting on — which is why
	// the contract's shutdown hold carries a schedule id at all.
	c.holdForSchedule(ctx, fields)

	c.wake()
	c.log.Info(opSchedule, "a drain is scheduled and announced", fields)
	return nil
}

// holdForSchedule takes the drain hold on every workspace, so intake is held
// from the moment the shutdown is announced.
func (c *controller) holdForSchedule(ctx context.Context, fields dlog.Context) {
	workspaces, err := c.deps.DB.ListWorkspaces(ctx)
	if err != nil {
		c.log.Error(opSchedule, "could not list the workspaces to hold", withCause(fields, err))
		return
	}
	taken := map[ids.WorkspaceID]ids.LeaseID{}
	for _, ws := range workspaces {
		lease, err := c.deps.DB.AcquireLease(ctx, ws.ID, wsm.HolderDrain, wsm.PolicyHold)
		if err != nil {
			// Another holder has the lease; its own policy already parks or
			// refuses intake, so the drain needs none of its own.
			c.log.Debug(opSchedule, "a workspace's lease is already held; leaving it as it stands",
				merge(fields, dlog.Context{"workspace": string(ws.ID), "cause": err.Error()}))
			continue
		}
		taken[ws.ID] = lease.ID
	}
	c.mu.Lock()
	c.scheduled = taken
	c.mu.Unlock()
	for ws := range taken {
		c.leaseChanged(ws)
	}
	c.log.Debug(opSchedule, "held the intake on every workspace for the standing schedule",
		merge(fields, dlog.Context{"workspaces": len(taken)}))
}

// releaseScheduleHolds drops the holds the standing schedule took.
func (c *controller) releaseScheduleHolds(ctx context.Context, operation string, fields dlog.Context) {
	c.mu.Lock()
	taken := c.scheduled
	c.scheduled = nil
	c.mu.Unlock()
	for ws, lease := range taken {
		if err := c.deps.DB.ReleaseLease(context.WithoutCancel(ctx), lease); err != nil {
			c.log.Warn(operation, "could not release a schedule's drain hold",
				merge(fields, dlog.Context{"workspace": string(ws), "lease": string(lease), "cause": err.Error()}))
			continue
		}
		c.leaseChanged(ws)
		c.publishHost(ws)
	}
}

// leaseChanged tells the prompt queue a workspace's lease set moved, which is
// what re-evaluates the holds taken against it.
func (c *controller) leaseChanged(ws ids.WorkspaceID) {
	if c.deps.LeaseChanged != nil {
		c.deps.LeaseChanged(ws)
	}
}

// publishHost recomposes the workspace's host view after this controller
// released its lease. The composer arm is a function of the lease, and the
// server never sees a release, so a client's last push would otherwise stand
// as `draining` after the drain is gone.
func (c *controller) publishHost(ws ids.WorkspaceID) {
	if c.deps.PublishHost != nil {
		c.deps.PublishHost(ws)
	}
}

// Cancel clears the schedule in force and announces the cancellation. Nothing
// scheduled is a REFUSAL: the contract has an arm for it.
func (c *controller) Cancel(ctx context.Context) error {
	current, err := c.deps.DB.DrainSchedule(ctx)
	if err != nil {
		c.log.Error(opCancel, "could not read the standing drain schedule", withCause(nil, err))
		return fmt.Errorf("drain: cancel: %w", err)
	}
	if current == nil {
		c.log.Warn(opCancel, "refused a cancel with nothing scheduled", nil)
		return ErrNothingScheduled
	}
	fields := dlog.Context{"schedule": ScheduleID(*current), "deadline": current.Deadline}
	if err := c.deps.DB.ClearDrainSchedule(ctx); err != nil {
		c.log.Error(opCancel, "could not clear the standing drain schedule", withCause(fields, err))
		return fmt.Errorf("drain: cancel: %w", err)
	}
	c.mu.Lock()
	c.fired = false
	c.mu.Unlock()

	c.releaseScheduleHolds(ctx, opCancel, fields)
	c.deps.Announcer.DrainCancelled(&agentreplv1.DaemonDrainCancelled{})
	c.wake()
	c.log.Info(opCancel, "the standing drain schedule was cancelled and the banner taken down", fields)
	return nil
}

// Current reports the schedule in force.
func (c *controller) Current(ctx context.Context) (*wsm.DrainSchedule, error) {
	current, err := c.deps.DB.DrainSchedule(ctx)
	if err != nil {
		c.log.Error(opCurrent, "could not read the standing drain schedule", withCause(nil, err))
		return nil, fmt.Errorf("drain: current: %w", err)
	}
	if current == nil {
		c.log.Debug(opCurrent, "no drain schedule is in force", nil)
		return nil, nil
	}
	c.log.Debug(opCurrent, "read the standing drain schedule",
		dlog.Context{"schedule": ScheduleID(*current), "deadline": current.Deadline})
	return current, nil
}

// ShutdownNow announces an immediate shutdown, STANDS EVERY SESSION DOWN, and
// exits. It takes no lease and waits for no freeness: the operator asked for
// now, and the in-flight WRITES — not the in-flight turns — are what the exit
// flushes.
//
// THE PROCESS TREE GOES WITH THE DAEMON. Every shim is spawned into a process
// GROUP OF ITS OWN so that a BOUNCE can hand it to a successor that adopts it
// (boot step "adopt"), and each of them holds two `shim-lock` children on the
// workspace's kernel claims. `now` has no successor — its announcement carries
// no address, which every client reads as a plain bounce — so a shim left
// standing here is nothing's to adopt: it holds the workspace lock that would
// refuse the next session, and its ~95 MiB stays resident for as long as the
// machine is up. Measured over one 24-scenario Emacs e2e run: 24 leaked shims
// and 48 leaked `shim-lock` holders, all of them downstream of this call.
//
// A REGISTERED SESSION AND AN IN-FLIGHT SPAWN GO DOWN SEPARATELY, in that
// order. standEverySessionDown walks the workspaces the STATE knows and stands
// each session down the ordinary way, through the fleet. It cannot reach a
// shim that was spawned and has not finished coming up: such a process enters
// the fleet's session map only after bring-up returns healthy AND StartSession
// answers, and Fleet.Stop answers nil for a workspace that is not in that map,
// so the walk steps straight past it and the daemon exits with the spawn still
// running. Measured: a shim spawned at 18:02:54.268 outlived a daemon whose
// serving lifetime ended 21 ms later, and was still holding the workspace lock
// and ~95 MiB when a 10 second grace expired. So the supervisor's own sweep
// runs AFTER the walk, over exactly the processes nothing else knew about.
//
// THE SWEEP IS THE `now` PATH'S ALONE. A BOUNCE must not kill its shims: they
// are what the successor adopts, and killing one would take the workspace's
// kernel lock down with it. Two things keep it out of the bounce. The bounce
// exits through rollout.controller.Handover, which never calls ShutdownNow at
// all; and each of its transfers calls Client.Detach, which leaves the process
// running and takes it OUT of the supervisor's registry, so even a sweep that
// somehow ran after a handover would find nothing of the successor's to kill.
//
// THIS IS THE ONE PLACE THE DRAIN FORCES. The package's standing ruling —
// teardown never interrupts the vendor — is about the SCHEDULED drain, which
// buys the shim's freeness by WAITING for it (`fire`, and the idle sweep's
// `KillSession(..., false)`). `now` bought nothing: the caller said now, and a
// graceful stand-down that waits on a turn parked at a permission gate would
// hang the very stop that is supposed to be immediate. So each stand-down is
// forced and BOUNDED by StandBound, and a workspace that will not go is
// REPORTED and stepped over rather than waited on — the exit below must happen
// whatever any one shim does.
func (c *controller) ShutdownNow(ctx context.Context, reason *agentreplv1.DrainReason) error {
	if reason == nil || reason.GetKind() == nil {
		err := errors.New("drain: an immediate shutdown states its reason")
		c.log.Error(opNow, "refused an immediate shutdown with no reason", withCause(nil, err))
		return err
	}
	c.deps.Announcer.ShutdownAnnounced(&agentreplv1.DaemonShutdownAnnounced{
		Cause: &agentreplv1.DaemonShutdownCause{
			Kind: &agentreplv1.DaemonShutdownCause_Immediate{
				Immediate: &agentreplv1.DaemonShutdownImmediate{Reason: reason},
			},
		},
		MintedAtMs: milliseconds(c.deps.Clock.Now()),
	})
	c.log.Info(opNow, "announced an immediate shutdown", nil)
	// THE LATCH GOES UP BEFORE ANYTHING IS ENDED, and that is the ordering
	// this line exists for. It used to be raised by the spawn sweep two lines
	// down, which is AFTER the walk below has already stood every registered
	// session down -- so every departure the walk itself caused landed while
	// the latch still read false, and any client the walk could not name read
	// this daemon's own teardown as a death. See supervisor.BeginStandDown.
	if c.deps.Spawns.BeginStandDown() {
		c.log.Debug(opNow, "latched the stand-down before any shim is ended", nil)
	}
	c.standEverySessionDown(ctx)
	c.sweepInFlightSpawns(ctx)
	if err := c.deps.Exit(ctx); err != nil {
		c.log.Error(opNow, "the orderly exit could not be started", withCause(nil, err))
		return fmt.Errorf("drain: shutdown now: %w", err)
	}
	return nil
}

// standEverySessionDown forces every workspace's shim down ahead of an
// immediate exit, one bounded call at a time.
//
// IT NEVER RETURNS AN ERROR, and that is deliberate rather than a swallowed
// one: every failure below is recorded at ERROR on the controller's own log,
// and none of them may stop the exit the operator asked for. A workspace whose
// stand-down failed is a leaked shim the caller cannot do anything about — the
// record is what makes it visible — while an exit skipped over one is a leaked
// DAEMON as well.
//
// It runs SERIALLY. The whole set is small (one shim per open workspace), each
// call is bounded by StandBound, and a forced stand-down of a healthy shim is
// single-digit milliseconds; a concurrent fan-out would buy nothing and would
// put N goroutines into the fleet's lock at the exact moment the process is
// tearing down.
//
// THE BOUND IS THE DAEMON'S OWN DECISION, NOT A SLICE OF THE CALLER'S BUDGET.
// Each stand-down runs on a context DETACHED from the caller's deadline
// (`context.WithoutCancel`), and the reason is that ShutdownNow's doc comment
// above is otherwise a promise this code cannot keep. Derived as a CHILD of
// ctx, the per-workspace bound is `min(StandBound, whatever the caller has
// left)` — so for any caller whose own budget is no larger than StandBound the
// caller's deadline always fires first, every workspace after the one that
// consumed the remainder is given up on with a context error before its shim
// was even asked, and "reported and stepped over" degrades into "skipped". The
// daemon's own e2e harness is exactly that caller: ONE 5s context, created at
// the daemon's process start and shared by every call the test makes, with
// `UpdateShutdownSchedule{now}` the last of them.
//
// DROPPING THE CALLER'S CANCELLATION TOO IS DELIBERATE, not an oversight. By
// the time this runs the immediate shutdown has already been ANNOUNCED to every
// WatchDaemon subscriber and the exit below is committed; a client that hangs
// up mid-stop must not be able to leave the shims standing, because a shim that
// outlives its daemon holds the workspace lock that refuses the next session
// and keeps ~95 MiB resident — the very leak this whole path exists to close.
// The caller's context bounds the ANSWER it is waiting for, never the act.
func (c *controller) standEverySessionDown(ctx context.Context) {
	workspaces, err := c.deps.DB.ListWorkspaces(ctx)
	if err != nil {
		c.log.Error(opNow, "could not list the workspaces to stand down; the shims will outlive this daemon",
			withCause(nil, err))
		return
	}
	for _, ws := range workspaces {
		fields := dlog.Context{"workspace": string(ws.ID)}
		standDown, cancel := context.WithTimeout(context.WithoutCancel(ctx), c.deps.StandBound)
		err := c.deps.Stand.KillSession(standDown, ws.ID, true)
		cancel()
		if err != nil {
			c.log.Error(opNow, "a session would not stand down before the immediate exit; its shim will outlive this daemon",
				withCause(fields, err))
			continue
		}
		c.log.Debug(opNow, "stood a session down ahead of the immediate exit", fields)
	}
}

// sweepInFlightSpawns stands down every shim process the supervisor started
// and still owns — the spawns that never reached the state the walk above
// reads, and which therefore have no workspace row to be stood down by.
//
// IT NEVER RETURNS AN ERROR, for the same reason standEverySessionDown does
// not: the failure is RECORDED at ERROR, and nothing may stop the exit the
// operator asked for. What it must never do is hide one, so the supervisor's
// joined failures are logged whole.
//
// THE BOUND IS StandBound (5s), the same budget one registered session's
// stand-down gets, and for the same reason: this runs on the request's own
// goroutine ahead of the exit, and an unbounded sweep is a daemon that does
// not go. It bounds the WHOLE sweep, while the supervisor additionally bounds
// each process by its own kill grace, so one unreapable child cannot spend the
// budget its siblings need. The set is normally EMPTY — a spawn is in flight
// for the few hundred milliseconds of one bring-up — and a forced kill of one
// that is there is a SIGKILL plus a reap, single-digit milliseconds, so 5s is
// three orders of magnitude of headroom over the work it actually does.
//
// IT IS DETACHED FROM THE CALLER'S DEADLINE for the same reason
// standEverySessionDown is, and the leak it guards is the worse of the two: an
// in-flight spawn has no workspace row, so nothing else in the daemon knows the
// process exists. A caller whose budget expired during the walk above would
// otherwise take this sweep's whole bound down with it, and the shim would be
// left running with nothing left that can name it.
func (c *controller) sweepInFlightSpawns(ctx context.Context) {
	sweep, cancel := context.WithTimeout(context.WithoutCancel(ctx), c.deps.StandBound)
	defer cancel()
	if err := c.deps.Spawns.StandDownEverySpawn(sweep, "an immediate shutdown was requested"); err != nil {
		c.log.Error(opNow, "a spawn in flight would not stand down before the immediate exit; its shim will outlive this daemon and hold the workspace lock",
			withCause(nil, err))
		return
	}
	c.log.Debug(opNow, "swept the spawns still in flight ahead of the immediate exit", nil)
}

// fire runs the scheduled drain: hold every workspace's intake, wait for each
// to fall free WITHOUT interrupting anything, announce, and exit.
func (c *controller) fire(ctx context.Context, s wsm.DrainSchedule) error {
	fields := dlog.Context{"schedule": ScheduleID(s), "deadline": s.Deadline}
	reason, err := DecodeReason(s.Reason)
	if err != nil {
		c.log.Error(opFire, "the standing schedule's reason will not decode", withCause(fields, err))
		return err
	}
	workspaces, err := c.deps.DB.ListWorkspaces(ctx)
	if err != nil {
		c.log.Error(opFire, "could not list the workspaces to drain", withCause(fields, err))
		return fmt.Errorf("drain: fire: %w", err)
	}
	c.log.Info(opFire, "the drain deadline passed; holding intake on every workspace",
		merge(fields, dlog.Context{"workspaces": len(workspaces)}))

	// THE HOLD FIRST, ON EVERY WORKSPACE, before any wait: a workspace whose
	// intake is still open while a sibling is being waited on would keep
	// accepting work the drain then has to wait for again.
	//
	// THE SCHEDULE'S OWN HOLDS ARE ALREADY THE HOLD. Schedule took the drain
	// lease on every workspace it could the moment the shutdown was announced
	// (holdForSchedule), so re-acquiring here asked the arbitration for a lease
	// this controller already held: every workspace of every drain came back
	// refused, as a WARN here and an ERROR in the store, for a hold that stood.
	c.mu.Lock()
	standing := c.scheduled
	c.mu.Unlock()
	held := make([]wsm.Lease, 0, len(workspaces))
	for _, ws := range workspaces {
		if lease, own := standing[ws.ID]; own {
			c.log.Debug(opFire, "the standing schedule's own drain hold already holds this workspace",
				merge(fields, dlog.Context{"workspace": string(ws.ID), "lease": string(lease)}))
			continue
		}
		lease, err := c.deps.DB.AcquireLease(ctx, ws.ID, wsm.HolderDrain, wsm.PolicyHold)
		var otherHolder *wsm.LeaseHeldError
		switch {
		case errors.As(err, &otherHolder):
			// Another holder has the lease; its own policy already parks or
			// refuses intake, so the drain does not need one of its own. That
			// is the arbitration answering, and waiting on it is the drain's
			// ordinary course.
			c.log.Info(opFire, "a workspace's lease is held by another holder; the drain will wait on it as it stands",
				merge(fields, dlog.Context{
					"workspace": string(ws.ID), "holder": otherHolder.Holder.String(), "lease": string(otherHolder.Lease),
				}))
			continue
		case err != nil:
			c.log.Error(opFire, "could not take the drain hold on a workspace; the drain waits on it unheld",
				merge(fields, dlog.Context{"workspace": string(ws.ID), "cause": err.Error()}))
			continue
		}
		held = append(held, lease)
		c.log.Debug(opFire, "took the drain hold on a workspace",
			merge(fields, dlog.Context{"workspace": string(ws.ID), "lease": string(lease.ID)}))
	}

	// THE HOLDS TAKEN HERE LIVE AS LONG AS THIS FIRE AND NO LONGER. A freeness
	// wait that ends early (the daemon leaving under it) returns without
	// reaching the ordinary release below, so the deferred one covers it, on a
	// context that cancellation cannot refuse.
	releaseHeld := func() {
		for _, lease := range held {
			if err := c.deps.DB.ReleaseLease(context.WithoutCancel(ctx), lease.ID); err != nil {
				c.log.Warn(opFire, "could not release a drain hold before exiting",
					merge(fields, dlog.Context{"lease": string(lease.ID), "cause": err.Error()}))
			}
		}
		held = nil
	}
	defer releaseHeld()

	for _, ws := range workspaces {
		if c.deps.Freeness.Free(ws.ID) {
			c.log.Debug(opFire, "a workspace was already free",
				merge(fields, dlog.Context{"workspace": string(ws.ID)}))
			continue
		}
		c.log.Info(opFire, "waiting for a workspace to fall free; nothing is interrupted",
			merge(fields, dlog.Context{"workspace": string(ws.ID)}))
		if err := c.deps.Freeness.AwaitFree(ctx, ws.ID); err != nil {
			c.log.Error(opFire, "the freeness wait ended before the workspace fell free",
				merge(fields, dlog.Context{"workspace": string(ws.ID), "cause": err.Error()}))
			return fmt.Errorf("drain: fire: wait for %q: %w", ws.ID, err)
		}
	}

	releaseHeld()

	// NO ADDRESS: a scheduled drain has no successor. Clients read the absent
	// address as a plain bounce and wait the outage out.
	c.deps.Announcer.ShutdownAnnounced(&agentreplv1.DaemonShutdownAnnounced{
		Cause: &agentreplv1.DaemonShutdownCause{
			Kind: &agentreplv1.DaemonShutdownCause_ScheduledDrain{
				ScheduledDrain: &agentreplv1.DaemonShutdownScheduledDrain{Reason: reason},
			},
		},
		MintedAtMs: milliseconds(c.deps.Clock.Now()),
	})
	c.log.Info(opFire, "every workspace is quiet; announced the scheduled shutdown", fields)
	if err := c.deps.Exit(ctx); err != nil {
		c.log.Error(opFire, "the orderly exit could not be started", withCause(fields, err))
		return fmt.Errorf("drain: fire: %w", err)
	}
	return nil
}

// wake nudges Run without blocking, so a schedule change is noticed at once.
func (c *controller) wake() {
	select {
	case c.rearm <- struct{}{}:
	default:
	}
}

// withCause stamps an error onto a record's context without mutating the
// caller's map.
func withCause(fields dlog.Context, err error) dlog.Context {
	out := merge(fields, nil)
	out["cause"] = err.Error()
	return out
}

// merge copies base and overlays extra, so no record shares a map with another.
func merge(base, extra dlog.Context) dlog.Context {
	out := make(dlog.Context, len(base)+len(extra)+1)
	for k, v := range base {
		out[k] = v
	}
	for k, v := range extra {
		out[k] = v
	}
	return out
}

// workspaceIDs is the id list of a workspace slice, for a record's context.
func workspaceIDs(in []ids.WorkspaceID) []string {
	out := make([]string, 0, len(in))
	for _, ws := range in {
		out = append(out, string(ws))
	}
	return out
}

// Republish re-arms the schedule a previous process left in force: the row is
// durable, but the daemon topic replays only what THIS process announced, so a
// reconnecting client would otherwise never learn a shutdown is still standing.
//
// It announces exactly what Schedule announced — the banner rendered from the
// persisted row — and takes the same intake holds, because the hold begins with
// the schedule and a restart does not end it.
func (c *controller) Republish(ctx context.Context) error {
	current, err := c.deps.DB.DrainSchedule(ctx)
	if err != nil {
		c.log.Error(opRepublish, "could not read the standing drain schedule at boot", withCause(nil, err))
		return fmt.Errorf("drain: republish: %w", err)
	}
	if current == nil {
		c.log.Debug(opRepublish, "no drain schedule survived the restart", nil)
		return nil
	}
	reason, err := DecodeReason(current.Reason)
	if err != nil {
		// A row that will not decode is corruption, not an absent schedule:
		// the boot refuses rather than serving as though nothing were standing.
		c.log.Error(opRepublish, "the persisted schedule's reason will not decode",
			withCause(dlog.Context{"deadline": current.Deadline}, err))
		return fmt.Errorf("drain: republish: %w", err)
	}
	fields := dlog.Context{
		"schedule": ScheduleID(*current),
		"deadline": current.Deadline,
		"set_at":   current.SetAt,
	}
	c.deps.Announcer.DrainScheduled(&agentreplv1.DaemonDrainScheduled{
		AtMs:   milliseconds(current.Deadline),
		Reason: reason,
	})
	c.holdForSchedule(ctx, fields)
	c.wake()
	c.log.Info(opRepublish, "a drain schedule survived the restart and was announced again", fields)
	return nil
}
