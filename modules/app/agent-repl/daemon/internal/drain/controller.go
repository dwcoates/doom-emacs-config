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
	opNew      = "daemon.drain.new"
	opSchedule = "daemon.drain.schedule"
	opCancel   = "daemon.drain.cancel"
	opCurrent  = "daemon.drain.current"
	opNow      = "daemon.drain.shutdown_now"
	opFire     = "daemon.drain.fire"
	opRefusal  = "daemon.drain.refusal"
	opSweep    = "daemon.drain.sweep"
	opRun      = "daemon.drain.run"
)

// IdleCutoffEnv compresses the idle cutoff for tests, in milliseconds. It BEATS
// the --idle-cutoff flag: a test that sets it must not also have to know how
// the daemon was launched.
const IdleCutoffEnv = "AGENT_REPL_HIBERNATE_IDLE_CUTOFF_MS"

// DefaultIdleCutoff is the cutoff when neither the flag nor the environment
// names one.
const DefaultIdleCutoff = 4 * time.Hour

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
	c.wake()
	c.log.Info(opSchedule, "a drain is scheduled and announced", fields)
	return nil
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

// ShutdownNow announces an immediate shutdown and exits. It takes no lease and
// waits for no freeness: the operator asked for now, and the in-flight WRITES —
// not the in-flight turns — are what the exit flushes.
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
	if err := c.deps.Exit(ctx); err != nil {
		c.log.Error(opNow, "the orderly exit could not be started", withCause(nil, err))
		return fmt.Errorf("drain: shutdown now: %w", err)
	}
	return nil
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
	held := make([]wsm.Lease, 0, len(workspaces))
	for _, ws := range workspaces {
		lease, err := c.deps.DB.AcquireLease(ctx, ws.ID, wsm.HolderDrain, wsm.PolicyHold)
		if err != nil {
			// Another holder has the lease; its own policy already parks or
			// refuses intake, so the drain does not need one of its own.
			c.log.Warn(opFire, "a workspace's lease is already held; the drain will wait on it as it stands",
				merge(fields, dlog.Context{"workspace": string(ws.ID), "cause": err.Error()}))
			continue
		}
		held = append(held, lease)
		c.log.Debug(opFire, "took the drain hold on a workspace",
			merge(fields, dlog.Context{"workspace": string(ws.ID), "lease": string(lease.ID)}))
	}

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

	for _, lease := range held {
		if err := c.deps.DB.ReleaseLease(ctx, lease.ID); err != nil {
			c.log.Warn(opFire, "could not release a drain hold before exiting",
				merge(fields, dlog.Context{"lease": string(lease.ID), "cause": err.Error()}))
		}
	}

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
