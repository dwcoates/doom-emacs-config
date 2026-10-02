package workspace

import (
	"context"
	"errors"
	"fmt"
	"math"
	"strconv"
	"time"

	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/health"
	"claude-repld/internal/resolve/sidebar"
	"claude-repld/internal/ids"
	"claude-repld/internal/rollout"
	"claude-repld/internal/shimclient"
	"claude-repld/internal/wsm"
)

// THE VENDOR-START RETRY RUN (docs/protobuf-design/vendor-start-resilience.md,
// landed change 2; owner rulings 1-5).
//
// THE SHIM LABELS, THE DAEMON LOOPS. A StartSession refusal carries the shim's
// verdict on whether asking again can help (shim.v1
// StartSessionVendorStartFailed.retry, and a fixed verdict per other arm); the
// daemon never re-derives it from the detail text. A RETRYABLE failure is asked
// again of the SAME shim on a multiplicative backoff -- x1.5 from 200ms,
// capped at 5s -- for ten minutes of wall time measured from the FIRST failure
// of the contiguous run. A successful start ends the run; the next failure
// opens a fresh window. A restart also opens a fresh one.
//
// The run lives in Fleet.startSession, the ONE place every bring-up passes
// through: a cold start (Fleet.start), a rollout relaunch (Fleet.Resume) and a
// cold-gate re-open (Fleet.ResumeCold).

const (
	// vendorRetryFloor is the wait before the first retry.
	vendorRetryFloor = 200 * time.Millisecond
	// vendorRetryCeiling caps every later wait.
	vendorRetryCeiling = 5 * time.Second
	// vendorRetryGrowth multiplies each wait into the next.
	vendorRetryGrowth = 1.5
	// DefaultVendorRetryWindow is how long a contiguous run of retryable
	// failures is retried, measured from its first failure (owner ruling 2).
	DefaultVendorRetryWindow = 10 * time.Minute
)

// vendorRetryDelay is the wait after the run's `failed`-th failure (1-based)
// before the next attempt: 200, 300, 450, 675, 1013, 1519, 2278, 3417, then
// 5000ms for every one after (owner ruling 1), rounded to the millisecond.
func vendorRetryDelay(failed uint32) time.Duration {
	if failed == 0 {
		failed = 1
	}
	ms := float64(vendorRetryFloor.Milliseconds()) * math.Pow(vendorRetryGrowth, float64(failed-1))
	if ms >= float64(vendorRetryCeiling.Milliseconds()) {
		return vendorRetryCeiling
	}
	return time.Duration(math.Round(ms)) * time.Millisecond
}

// ErrVendorStartCancelled is the cause a vendor-start run ends with when a
// restart cancels it (Fleet.CancelVendorStart). It wraps
// rollout.ErrResumeRestarted, so a relaunch whose resume it ended stands its
// shim down and relaunches rather than failing.
var ErrVendorStartCancelled = fmt.Errorf("workspace: the vendor-start retry run was ended by a restart: %w", rollout.ErrResumeRestarted)

// startLabel is the verdict a StartSession REFUSAL carries: whether asking
// again can help, and whether it was the vendor that failed (rather than the
// shim's own lock helper or the conversation's ownership). It wraps the
// refusal, so the typed Refusal under it still reaches the transport.
type startLabel struct {
	// retryable is the shim's verdict: true when the same request may succeed
	// if asked again.
	retryable bool
	// vendor is true when the vendor (the agent binary or its SDK query) is
	// what did not start: the three vendor fault kinds describe only that.
	vendor bool
	// cause is the shim's own account, drawn verbatim.
	cause string
	err   error
}

func (l *startLabel) Error() string { return l.err.Error() }
func (l *startLabel) Unwrap() error { return l.err }

// labeled wraps a refusal with its verdict.
func labeled(err error, retryable, vendor bool, cause string) error {
	return &startLabel{retryable: retryable, vendor: vendor, cause: cause, err: err}
}

// vendorRun is one workspace's vendor-start run state, held in memory by the
// fleet (design record: "the run's anchor ... is held by the fleet in
// memory").
type vendorRun struct {
	// since is the run's anchor, the first failure of the contiguous run;
	// zero while no run of failures stands.
	since time.Time
	// failed counts the run's failed attempts.
	failed uint32
	// retrying is the standing `vendor_start_retrying` fault, "" when none.
	retrying ids.FaultID
	// terminal is the standing `vendor_start_rejected` or
	// `vendor_start_failed` fault this fleet filed, "" when none. A later
	// terminal fault REPLACES it, so a revival that meets the same spent
	// window does not stack a second one.
	terminal ids.FaultID
	// cancel ends the bring-up in flight that is asking, nil when none is;
	// done is closed when that bring-up has returned.
	cancel context.CancelCauseFunc
	done   chan struct{}
}

// vendorRunLocked answers the workspace's run state, minting it. The caller
// holds f.mu.
func (f *Fleet) vendorRunLocked(ws ids.WorkspaceID) *vendorRun {
	run, ok := f.vendorRuns[ws]
	if !ok {
		run = &vendorRun{}
		f.vendorRuns[ws] = run
	}
	return run
}

// beginVendorStart registers the bring-up about to ask the shim to start the
// workspace's session, so a restart can end it (CancelVendorStart). The
// returned context carries that cancellation; the returned func is called
// when the bring-up has RETURNED -- whatever cleanup it does after its failed
// start included -- and is what CancelVendorStart waits on.
func (f *Fleet) beginVendorStart(ctx context.Context, ws ids.WorkspaceID) (context.Context, func()) {
	runCtx, cancel := context.WithCancelCause(ctx)
	done := make(chan struct{})
	f.mu.Lock()
	run := f.vendorRunLocked(ws)
	run.cancel, run.done = cancel, done
	f.mu.Unlock()
	return runCtx, func() {
		f.mu.Lock()
		if run.done == done {
			run.cancel, run.done = nil, nil
		}
		f.mu.Unlock()
		cancel(nil)
		close(done)
	}
}

// CancelVendorStart implements Sessions: it ends the bring-up asking the shim
// to start the workspace's session, if one is, and waits (bounded by ctx) for
// it to return. It also clears the run's anchor, because a restart begins a
// fresh window (owner ruling 2).
func (f *Fleet) CancelVendorStart(ctx context.Context, ws ids.WorkspaceID) bool {
	f.mu.Lock()
	run := f.vendorRunLocked(ws)
	cancel, done := run.cancel, run.done
	run.since, run.failed = time.Time{}, 0
	f.mu.Unlock()
	if cancel == nil {
		return false
	}
	cancel(ErrVendorStartCancelled)
	select {
	case <-done:
	case <-ctx.Done():
		f.deps.Log.Global().Error(opBringUp, "the cancelled vendor-start run did not return before the restart's context ended", dlog.Context{
			"workspace": string(ws), "cause": ctx.Err().Error(),
		})
	}
	return true
}

// startSession asks the shim to start the workspace's session, retrying a
// RETRYABLE refusal on the run's backoff, and files a fault for every way the
// asking can end short of a session.
//
// THE FAULT IS FILED HERE, AT THE ONE SITE: a vendor that did not start files
// the three vendor kinds; every other refusal files `resume_failed`
// (noteSessionRefused). The two conditions that are NOT failures -- a cold
// gate, which is a designed product state the user answers, and a stand-down
// this daemon ordered -- pass through unfiled. A transport error (the shim
// died under the call) is not retried here: the shim's death is its own edge.
func (f *Fleet) startSession(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID, client shimclient.Client, src source, session wsm.Session, configDir string) (*conversationv1.SessionStarted, error) {
	for {
		started, err := f.askToStartSession(ctx, log, ws, client, src, session, configDir)
		if err == nil {
			// A SESSION THAT IS SERVING is the repair of every fault whose
			// lifetime ends at a started session -- the vendor-start faults
			// among them -- and ends the run. A COLD answer is an answer too:
			// it ends the run, and the retry it stood for is over.
			f.endVendorRun(ctx, log, ws)
			if started != nil {
				f.closeOnEdge(ctx, log, ws, health.EdgeSessionStarted)
			}
			return started, nil
		}
		if errors.Is(err, shimclient.ErrStandDownOrdered) {
			return nil, err
		}
		if cause := context.Cause(ctx); errors.Is(cause, ErrVendorStartCancelled) {
			f.closeRetrying(ctx, log, ws)
			log.Info(opBringUp, "the vendor-start run was ended by a restart", dlog.Context{"cause": err.Error()})
			return nil, fmt.Errorf("start session for %q: %w", ws, ErrVendorStartCancelled)
		}
		var label *startLabel
		if !errors.As(err, &label) {
			f.closeRetrying(ctx, log, ws)
			f.noteSessionRefused(ctx, log, ws, err)
			return nil, err
		}
		if !label.retryable {
			f.closeRetrying(ctx, log, ws)
			if label.vendor {
				f.noteVendorRejected(ctx, log, ws, label.cause)
			} else {
				f.noteSessionRefused(ctx, log, ws, err)
			}
			return nil, err
		}
		delay, retry := f.noteRetryableFailure(ctx, log, ws, label.cause)
		if !retry {
			return nil, err
		}
		select {
		case <-f.after(delay):
		case <-ctx.Done():
			if errors.Is(context.Cause(ctx), ErrVendorStartCancelled) {
				f.closeRetrying(ctx, log, ws)
				log.Info(opBringUp, "the vendor-start run was ended by a restart while it waited to retry", nil)
				return nil, fmt.Errorf("start session for %q: %w", ws, ErrVendorStartCancelled)
			}
			// THE DAEMON IS STANDING DOWN, or the caller gave up: the run
			// ends with the context, and the retrying fault is closed so no
			// surface keeps saying a retry is coming.
			f.closeRetrying(ctx, log, ws)
			log.Info(opBringUp, "the vendor-start run ended with its context while it waited to retry", dlog.Context{
				"cause": context.Cause(ctx).Error(),
			})
			return nil, fmt.Errorf("start session for %q: %w (after %w)", ws, context.Cause(ctx), err)
		}
	}
}

// noteRetryableFailure records one retryable failure of the run: it anchors a
// new run, counts the attempt, and either REPLACES the retrying fault and
// answers the wait before the next attempt, or -- the window spent -- closes
// the retrying fault, files `vendor_start_failed`, and answers false.
func (f *Fleet) noteRetryableFailure(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID, cause string) (time.Duration, bool) {
	now := f.now()
	f.mu.Lock()
	run := f.vendorRunLocked(ws)
	if run.since.IsZero() {
		run.since = now
	}
	run.failed++
	since, failed, previous := run.since, run.failed, run.retrying
	f.mu.Unlock()

	evidence := map[string]string{
		health.EvidenceFailedAttempts: strconv.FormatUint(uint64(failed), 10),
		health.EvidenceCause:          cause,
		health.EvidenceFailingSinceMs: strconv.FormatInt(since.UnixMilli(), 10),
	}
	fields := dlog.Context{"failed_attempts": failed, "cause": cause, "failing_since": since, "window": f.vendorWindow.String()}
	if now.Sub(since) >= f.vendorWindow {
		f.closeRetrying(ctx, log, ws)
		log.Error(opBringUp, "the vendor kept failing to start for the whole retry window; nothing retries until a restart", fields)
		f.openTerminal(ctx, log, ws, health.KindVendorStartFailed,
			"the vendor failed to start for the whole retry window", evidence)
		return 0, false
	}
	delay := vendorRetryDelay(failed)
	fields["retry_in_ms"] = delay.Milliseconds()
	// THE RETRYING FAULT IS REPLACED: the new one is opened, then the old one
	// closed, so a surface never reads the gap between them as recovery.
	id, opened := f.openVendorFault(ctx, log, ws, health.KindVendorStartRetrying,
		"the vendor did not start; retrying", evidence)
	if opened {
		f.mu.Lock()
		run.retrying = id
		f.mu.Unlock()
		if previous != "" {
			f.closeFault(ctx, log, previous)
		}
		f.noteRosterVendor(ws)
	}
	// A RETRY IS THE MECHANISM WORKING, not a fault in the daemon: INFO. The
	// fault is what the user reads; the record is the operator's.
	log.Info(opBringUp, "the vendor did not start; retrying on the backoff", fields)
	return delay, true
}

// noteVendorRejected files `vendor_start_rejected`: the vendor refused the
// start for a reason asking again cannot fix.
func (f *Fleet) noteVendorRejected(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID, cause string) {
	log.Error(opBringUp, "the vendor refused to start; nothing retries until a restart", dlog.Context{"cause": cause})
	f.openTerminal(ctx, log, ws, health.KindVendorStartRejected,
		"the vendor refused to start", map[string]string{health.EvidenceCause: cause})
}

// openTerminal files a terminal vendor-start fault, replacing the one this
// fleet filed before it (opened first, then the old one closed).
func (f *Fleet) openTerminal(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID, kind, detail string, evidence map[string]string) {
	id, opened := f.openVendorFault(ctx, log, ws, kind, detail, evidence)
	if !opened {
		return
	}
	f.mu.Lock()
	run := f.vendorRunLocked(ws)
	previous := run.terminal
	run.terminal = id
	f.mu.Unlock()
	if previous != "" {
		f.closeFault(ctx, log, previous)
	}
	f.noteRosterVendor(ws)
}

// openVendorFault records one vendor-start fault and republishes the host
// view. A cancelled context is a daemon standing down, not a fault to file.
func (f *Fleet) openVendorFault(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID, kind, detail string, evidence map[string]string) (ids.FaultID, bool) {
	if ctx.Err() != nil {
		return "", false
	}
	workspace := ws
	id, err := f.deps.DB.OpenFault(ctx, wsm.Fault{
		Workspace: &workspace,
		Kind:      kind,
		Detail:    detail,
		Evidence:  evidence,
		OpenedAt:  f.now(),
	})
	if err != nil {
		log.Error(opBringUp, "could not record the vendor-start fault", dlog.Context{"kind": kind, "cause": err.Error()})
		return "", false
	}
	f.publishHost(ws)
	return id, true
}

// closeRetrying closes the run's standing retrying fault, if one stands. It
// writes on a context the caller's cancellation cannot reach: a run ended by a
// restart or a stand-down must not leave a fault saying a retry is coming.
func (f *Fleet) closeRetrying(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID) {
	f.mu.Lock()
	run := f.vendorRunLocked(ws)
	id := run.retrying
	run.retrying = ""
	f.mu.Unlock()
	if id == "" {
		return
	}
	f.closeFault(context.WithoutCancel(ctx), log, id)
	f.noteRosterVendor(ws)
	f.publishHost(ws)
}

// closeFault closes one fault by id, loudly when it cannot.
func (f *Fleet) closeFault(ctx context.Context, log dlog.Logger, id ids.FaultID) {
	if err := f.deps.DB.CloseFault(ctx, id, f.now()); err != nil {
		log.Error(opBringUp, "could not close the replaced vendor-start fault", dlog.Context{"fault": string(id), "cause": err.Error()})
	}
}

// endVendorRun ends the run on an answer: the anchor and the count are
// cleared, so the next failure opens a fresh window, and the retrying fault is
// closed (a started session's edge closes it too; a cold answer has no such
// edge).
func (f *Fleet) endVendorRun(ctx context.Context, log dlog.Logger, ws ids.WorkspaceID) {
	f.mu.Lock()
	run := f.vendorRunLocked(ws)
	failed := run.failed
	run.since, run.failed = time.Time{}, 0
	// A started session's edge closes the terminal fault (its lifetime);
	// the fleet forgets it so it is never closed twice.
	hadTerminal := run.terminal != ""
	run.terminal = ""
	f.mu.Unlock()
	f.closeRetrying(ctx, log, ws)
	if hadTerminal {
		f.noteRosterVendor(ws)
	}
	if failed > 0 {
		log.Info(opBringUp, "the vendor started after failed attempts; the retry run is over", dlog.Context{"failed_attempts": failed})
	}
}

// noteRosterVendor tells the roster where the run stands, read off the faults
// the fleet holds standing for it: a retrying fault is a run being retried, a
// terminal one a stopped run, neither no run at all.
func (f *Fleet) noteRosterVendor(ws ids.WorkspaceID) {
	f.mu.Lock()
	run := f.vendorRunLocked(ws)
	state := sidebar.VendorStartNone
	switch {
	case run.retrying != "":
		state = sidebar.VendorStartRetrying
	case run.terminal != "":
		state = sidebar.VendorStartStopped
	}
	f.mu.Unlock()
	f.deps.VendorStarts(ws, state)
}
