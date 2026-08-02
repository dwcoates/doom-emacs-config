package sessioncontroller

import (
	"context"
	"fmt"
	"time"

	"claude-repld/internal/workspace/merge"
)

// This file is the session controller's half of SHIM-DRIVEN CONFLICT
// RESOLUTION: merge.Coordinator parks a conflicted cherry-pick, and the agent
// session that wrote the conflicting commits is asked to resolve it.
//
// The merge subsystem declares the port (merge.ConflictResolver) and this
// package implements it, never the reverse: merge must not import the session
// controller, whose merge lease exists to serve merge.Coordinator in the first
// place.
//
// THE CONTRACT IS "RETURNS WHEN THE TURN HAS ENDED", and the whole of the
// machinery below exists to make that statement true rather than approximately
// true. merge.Coordinator resumes the cherry-pick the instant Resolve returns,
// so resolving early — on some other turn's end — would continue a pick over a
// tree the agent is still editing.

// mergeResolutionTurnBound is how long one conflict-resolution turn may run
// before the wait gives up.
//
// It is a BOUND, not a retry budget and not a guess at how long resolution
// takes: the merge holds the workspace's shim lease for the whole of this wait,
// so a turn that never reports its end would otherwise hold that session
// forever with no user able to prompt it. Expiry is reported as the error it
// is, and merge.Coordinator leaves the conflict parked for a human.
const mergeResolutionTurnBound = 30 * time.Minute

// turnWaiter is one wait for a specific turn to END.
//
// It is armed BEFORE the prompt is submitted and correlated to a turn ID
// afterwards: the first turn to START while the waiter is armed is the turn the
// submit produced (nothing else may submit to this session — the merge lease
// refuses every user prompt while it is held), and only THAT turn's end
// completes the wait. A turn already running when the waiter arms — a user turn
// whose interrupt-driven end is still in flight — started before arming, so its
// end matches no waiter and cannot wake this one.
type turnWaiter struct {
	// turnID is the turn this waiter is bound to, valid once bound is true.
	turnID string
	bound  bool
	// done is closed by the matching TurnEnded, exactly once.
	done chan struct{}
}

// armTurnWaiter registers an unbound waiter on d. Caller must NOT hold m.mu.
func (m *Manager) armTurnWaiter(d *sessionController) *turnWaiter {
	w := &turnWaiter{done: make(chan struct{})}
	m.mu.Lock()
	d.turnWaiters = append(d.turnWaiters, w)
	m.mu.Unlock()
	return w
}

// dropTurnWaiter removes w from d, whether or not it ever fired. Idempotent, so
// the deferred cleanup of a completed wait is harmless.
func (m *Manager) dropTurnWaiter(d *sessionController, w *turnWaiter) {
	m.mu.Lock()
	defer m.mu.Unlock()
	kept := d.turnWaiters[:0]
	for _, existing := range d.turnWaiters {
		if existing != w {
			kept = append(kept, existing)
		}
	}
	d.turnWaiters = kept
}

// onTurnEvent binds and completes this session's turn waiters.
//
// Called on the shim read-loop goroutine for EVERY accepted turn boundary, so
// it does nothing but bookkeeping and a channel close — never a send that needs
// that loop to make progress.
func (m *Manager) onTurnEvent(d *sessionController, started bool, turnID string) {
	m.mu.Lock()
	var completed []*turnWaiter
	var bound int
	kept := d.turnWaiters[:0]
	for _, w := range d.turnWaiters {
		switch {
		case started && !w.bound:
			w.turnID, w.bound = turnID, true
			bound++
			kept = append(kept, w)
		case !started && w.bound && w.turnID == turnID:
			completed = append(completed, w)
		default:
			kept = append(kept, w)
		}
	}
	d.turnWaiters = kept
	waiting := len(kept)
	m.mu.Unlock()

	for _, w := range completed {
		close(w.done)
	}
	if bound == 0 && len(completed) == 0 {
		return
	}
	m.logf("session-controller: turn waiter bookkeeping ws=%q session=%s started=%v turn_id=%q bound=%d completed=%d still_waiting=%d",
		d.workspace, d.sessionID, started, turnID, bound, len(completed), waiting)
}

// SubmitMergePromptAwaitingTurn submits a prompt on behalf of the merge lease
// holder and returns ONCE THE TURN IT STARTED HAS ENDED.
//
// The waiter is armed BEFORE the submit, which is what makes a turn that ends
// almost immediately still observable: arming afterwards would race the very
// boundary being waited for.
//
// It deliberately uses existing rather than ensure: a merge prompt is only ever
// admissible against a session the lease was taken over, and a workspace with no
// live session controller is a loud failure rather than a reason to spawn one.
func (m *Manager) SubmitMergePromptAwaitingTurn(ctx context.Context, workspace, requestID, text, permissionMode string) error {
	d, err := m.existing(workspace)
	if err != nil {
		m.logf("session-controller: merge resolution prompt ws=%q request_id=%s has NO live session controller: %v", workspace, requestID, err)
		return fmt.Errorf("session-controller: merge resolution prompt for workspace %q: %w", workspace, err)
	}
	w := m.armTurnWaiter(d)
	defer m.dropTurnWaiter(d, w)

	if err := m.SubmitMergePrompt(ctx, workspace, requestID, text, permissionMode); err != nil {
		return err
	}
	select {
	case <-w.done:
		m.logf("session-controller: merge resolution turn ENDED ws=%q session=%s request_id=%s turn_id=%q",
			workspace, d.sessionID, requestID, m.turnWaiterID(w))
		return nil
	case <-ctx.Done():
		err := fmt.Errorf("session-controller: the merge resolution turn for workspace %q (request %s) never reported its end: %w",
			workspace, requestID, ctx.Err())
		m.logf("session-controller: merge resolution turn UNFINISHED ws=%q session=%s request_id=%s bound_turn_id=%q: %v — the cherry-pick is NOT resumed",
			workspace, d.sessionID, requestID, m.turnWaiterID(w), err)
		return err
	}
}

// turnWaiterID reads w's bound turn id under the manager mutex, for logging.
func (m *Manager) turnWaiterID(w *turnWaiter) string {
	m.mu.Lock()
	defer m.mu.Unlock()
	return w.turnID
}

// mergeResolutionPermissionMode is the permission mode the resolution prompt is
// submitted under.
//
// EMPTY IS DELIBERATE: it leaves the session on whatever mode its user chose. A
// merge borrows a session, and borrowing it must not quietly widen what the
// agent may do in it. A resolution that stalls on a permission the user is not
// there to grant is reported as an unfinished turn, and the conflict stays
// parked — which is the honest outcome, not a reason to escalate.
const mergeResolutionPermissionMode = ""

// ResolveMergeConflict implements merge.ConflictResolver (through the server's
// PromptRouter): it submits the resolution prompt on the merge lease holder's
// behalf and returns once that turn has ended.
//
// *Manager IS the adapter rather than a separate object wrapping one, because
// the session it must drive is the session it already routes the user's prompts
// to. Handing merge.Coordinator anything else would let the conflict be resolved
// on one fleet while the lease stands over another.
//
// What the agent is TOLD to do is merge-subsystem knowledge and lives with the
// port (merge.ConflictResolution.Prompt); what a completed turn IS lives here.
func (m *Manager) ResolveMergeConflict(ctx context.Context, res merge.ConflictResolution) error {
	if res.Workspace == "" {
		return fmt.Errorf("session-controller: a merge conflict resolution needs a workspace")
	}
	if res.RequestID == "" {
		return fmt.Errorf("session-controller: a merge conflict resolution for workspace %q needs a request id", res.Workspace)
	}
	m.logf("session-controller: merge conflict resolution ws=%q request_id=%s commit=%s branch=%s target=%q — driving the workspace's own session under the merge lease",
		res.Workspace, res.RequestID, res.ConflictCommit, res.SourceBranch, res.TargetDir)

	ctx, cancel := context.WithTimeout(ctx, m.mergeResolutionBound())
	defer cancel()
	if err := m.SubmitMergePromptAwaitingTurn(ctx, res.Workspace, res.RequestID, res.Prompt(), mergeResolutionPermissionMode); err != nil {
		m.logf("session-controller: merge conflict resolution FAILED ws=%q request_id=%s commit=%s: %v",
			res.Workspace, res.RequestID, res.ConflictCommit, err)
		return err
	}
	m.logf("session-controller: merge conflict resolution turn COMPLETE ws=%q request_id=%s commit=%s — merge.Coordinator resumes the cherry-pick",
		res.Workspace, res.RequestID, res.ConflictCommit)
	return nil
}

// ResolveMergeTestFailure implements merge.TestFailureResolver (through the
// server's PromptRouter): it submits the test-fix prompt on the merge lease
// holder's behalf and returns once that turn has ended.
//
// It is the sibling of ResolveMergeConflict and shares every part of its
// machinery — the same lease, the same session, the same turn-boundary wait,
// the same bound. The two differ only in what the agent is told, which is
// merge-subsystem knowledge and lives with the port
// (merge.TestFailureResolution.Prompt).
func (m *Manager) ResolveMergeTestFailure(ctx context.Context, res merge.TestFailureResolution) error {
	if res.Workspace == "" {
		return fmt.Errorf("session-controller: a merge test-failure resolution needs a workspace")
	}
	if res.RequestID == "" {
		return fmt.Errorf("session-controller: a merge test-failure resolution for workspace %q needs a request id", res.Workspace)
	}
	m.logf("session-controller: merge test-failure resolution ws=%q request_id=%s commit=%s branch=%s target=%q — driving the workspace's own session under the merge lease",
		res.Workspace, res.RequestID, res.FailingCommit, res.SourceBranch, res.TargetDir)

	ctx, cancel := context.WithTimeout(ctx, m.mergeResolutionBound())
	defer cancel()
	if err := m.SubmitMergePromptAwaitingTurn(ctx, res.Workspace, res.RequestID, res.Prompt(), mergeResolutionPermissionMode); err != nil {
		m.logf("session-controller: merge test-failure resolution FAILED ws=%q request_id=%s commit=%s: %v",
			res.Workspace, res.RequestID, res.FailingCommit, err)
		return err
	}
	m.logf("session-controller: merge test-failure resolution turn COMPLETE ws=%q request_id=%s commit=%s — merge.Coordinator commits the fix and re-runs the suite",
		res.Workspace, res.RequestID, res.FailingCommit)
	return nil
}

// RunMergeBeforeAction implements merge.BeforeActionRunner (through the server's
// PromptRouter): it submits the workspace's recorded before_ws_merge action on
// the merge lease holder's behalf and returns once that turn has ended.
//
// It is the third sibling of ResolveMergeConflict and ResolveMergeTestFailure and
// shares all of their machinery — the same lease, the same session, the same
// turn-boundary wait, the same bound. "DONE" IS THE TURN ENDING, never a sleep
// or a poll, which is what lets merge.Coordinator compute the cherry-pick plan
// knowing the action's commits (if any) are already in the branch.
func (m *Manager) RunMergeBeforeAction(ctx context.Context, act merge.BeforeAction) error {
	if act.Workspace == "" {
		return fmt.Errorf("session-controller: a merge before-action needs a workspace")
	}
	if act.RequestID == "" {
		return fmt.Errorf("session-controller: a merge before-action for workspace %q needs a request id", act.Workspace)
	}
	if act.Prompt == "" {
		return fmt.Errorf("session-controller: a merge before-action for workspace %q needs a prompt", act.Workspace)
	}
	m.logf("session-controller: merge before-action ws=%q request_id=%s — driving the workspace's own session under the merge lease before the cherry-pick plan is computed",
		act.Workspace, act.RequestID)

	ctx, cancel := context.WithTimeout(ctx, m.mergeResolutionBound())
	defer cancel()
	if err := m.SubmitMergePromptAwaitingTurn(ctx, act.Workspace, act.RequestID, act.Prompt, mergeResolutionPermissionMode); err != nil {
		m.logf("session-controller: merge before-action FAILED ws=%q request_id=%s: %v", act.Workspace, act.RequestID, err)
		return err
	}
	m.logf("session-controller: merge before-action turn COMPLETE ws=%q request_id=%s — merge.Coordinator computes the cherry-pick plan now",
		act.Workspace, act.RequestID)
	return nil
}

// mergeResolutionBound resolves the wait's bound: the configured override when
// a harness set one, the package default otherwise.
func (m *Manager) mergeResolutionBound() time.Duration {
	if m.cfg.MergeResolutionTurnBound > 0 {
		return m.cfg.MergeResolutionTurnBound
	}
	return mergeResolutionTurnBound
}
