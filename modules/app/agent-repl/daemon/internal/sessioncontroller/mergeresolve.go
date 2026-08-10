package sessioncontroller

import (
	"context"
	"fmt"
	"time"

	corev1 "agentrepl/proto/agentshim/core/v1"

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

// mergeResolutionTurnBindBound is how long a submitted merge prompt may go
// without a turn STARTING for it.
//
// IT EXISTS BECAUSE THE 30-MINUTE BOUND IS SIZED FOR AN AGENT THAT IS WORKING,
// and a turn that never began is not an agent that is working — it is an agent
// that was never asked. A test-fix prompt once sat parked on a session's queue
// behind a user turn whose end never arrived; the wait bound it to nothing and
// burned all thirty minutes before the pipeline could take its rollback path,
// with `bound_turn_id=""` the only trace that no turn had ever existed.
//
// So the wait is TWO PHASES: the turn must START within this bound, and only a
// turn that started gets the long one. Two minutes is generous for the distance
// between a forwarded prompt and the shim's TurnStarted — that boundary is one
// round trip, not one unit of work — while still costing a broken merge minutes
// instead of half an hour.
const mergeResolutionTurnBindBound = 2 * time.Minute

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
	// boundDone is closed by the TurnStarted that binds this waiter, exactly
	// once. It is what makes "a turn started for my prompt" observable on its
	// own, separately from "that turn ended" — the two phases of the wait have
	// different deadlines and different failure causes.
	boundDone chan struct{}
	// outcome is what the matching TurnEnded said about itself. Written before
	// done is closed and read only after, so the close is its handoff.
	outcome turnOutcome
	// done is closed by the matching TurnEnded, exactly once.
	done chan struct{}
}

// turnOutcome is a completed turn's own verdict on itself.
//
// IT EXISTS BECAUSE "THE TURN ENDED" IS NOT "THE TURN WORKED". A merge action is
// a turn the run demanded — the user's stated precondition for merging, or the
// postprocessing they asked to follow it — and a waiter that reported success
// for any end at all told merge.Coordinator an errored action had completed. The
// before-action's failure then landed a plan the action was meant to change, and
// the after-action's never reached after_action_error at all.
type turnOutcome struct {
	// isError is TurnEnded.is_error: the turn ended badly.
	isError bool
	// stopReason is TurnEnded.stop_reason, the vendor's own word for why.
	stopReason string
}

// armTurnWaiter registers an unbound waiter on d. Caller must NOT hold m.mu.
func (m *Manager) armTurnWaiter(d *sessionController) *turnWaiter {
	w := &turnWaiter{done: make(chan struct{}), boundDone: make(chan struct{})}
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
func (m *Manager) onTurnEvent(d *sessionController, started bool, turnID string, outcome turnOutcome) {
	m.mu.Lock()
	var completed, newlyBound []*turnWaiter
	var bound int
	kept := d.turnWaiters[:0]
	for _, w := range d.turnWaiters {
		switch {
		case started && !w.bound:
			w.turnID, w.bound = turnID, true
			bound++
			newlyBound = append(newlyBound, w)
			kept = append(kept, w)
		case !started && w.bound && w.turnID == turnID:
			// Under m.mu and before the close below, which is what publishes it
			// to the waiting goroutine.
			w.outcome = outcome
			completed = append(completed, w)
		default:
			kept = append(kept, w)
		}
	}
	d.turnWaiters = kept
	waiting := len(kept)
	m.mu.Unlock()

	// Both closes happen outside m.mu, like `done` always has: this runs on the
	// shim read loop. No waiter is ever in both slices — the switch above is
	// exclusive on `started` — so a waiter's bind is published by the TurnStarted
	// call and its end by the later TurnEnded one, in that order.
	for _, w := range newlyBound {
		close(w.boundDone)
	}

	// THE BARE `/model`'s OWN BOUNDARY. The CLI resolves `/model` inside a
	// turn, so that turn's end is the instant its answer exists — which makes
	// this a rendezvous rather than a delay long enough to probably work
	// (modelreadback.go). Claimed exactly once, so a re-observed end cannot ask
	// the shim twice.
	if !started && m.takeModelReadback(d, turnID) {
		m.readBackObservedModel(d, turnID)
	}

	for _, w := range completed {
		close(w.done)
	}
	if bound == 0 && len(completed) == 0 {
		return
	}
	m.logf("session-controller: turn waiter bookkeeping ws=%q session=%s started=%v turn_id=%q is_error=%v stop_reason=%q bound=%d completed=%d still_waiting=%d",
		d.workspace, d.sessionID, started, turnID, outcome.isError, outcome.stopReason, bound, len(completed), waiting)
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
//
// THE WAIT IS TWO PHASES, BIND THEN WORK, and the split is the whole point. The
// long bound is a budget for an AGENT THAT IS WORKING; spending it on a turn
// that never started spends it on nothing. So the prompt must produce a
// TurnStarted within mergeResolutionTurnBindBound, and only after that does the
// long bound apply. Every failure names the submit's own disposition, because a
// merge that fails on "the turn never began" is otherwise indistinguishable in
// the record from one that fails on "the agent went quiet".
func (m *Manager) SubmitMergePromptAwaitingTurn(ctx context.Context, workspace, requestID, text, permissionMode string, promptOrigin corev1.PromptOrigin) error {
	d, err := m.existing(workspace)
	if err != nil {
		m.logf("session-controller: merge resolution prompt ws=%q request_id=%s has NO live session controller: %v", workspace, requestID, err)
		return fmt.Errorf("session-controller: merge resolution prompt for workspace %q: %w", workspace, err)
	}
	w := m.armTurnWaiter(d)
	defer m.dropTurnWaiter(d, w)

	disposition, err := m.submitMergePrompt(ctx, workspace, requestID, text, permissionMode, promptOrigin)
	if err != nil {
		return err
	}
	// A PARKED MERGE PROMPT IS A BUSY WORKSPACE, and it is decided here rather
	// than waited out. The lease's precondition is that nothing of the user's is
	// running behind this shim; a prompt that reached the QUEUE says that
	// precondition does not hold — the session still has a turn in flight, and
	// the merge would be competing with the user's own agent for it. No bind
	// deadline can improve that answer, so the gate fails now with the reason,
	// and the parked prompt is taken back with it.
	if disposition.queued() {
		if cancelErr := m.cancelQueueEntry(workspace, disposition.queuedEntryID, "merge-gate", "cancelled_by_merge_gate"); cancelErr != nil {
			m.logf("session-controller: merge resolution prompt could NOT be taken back off the queue ws=%q request_id=%s entry=%s: %v — it may still be delivered to the agent after the merge has been failed",
				workspace, requestID, disposition.queuedEntryID, cancelErr)
		}
		err := fmt.Errorf("session-controller: the merge resolution turn for workspace %q (request %s) never started: the workspace is BUSY with a turn of the user's own, so %s; a merge resolution cannot run on a session the user's agent is working in",
			workspace, requestID, disposition)
		m.logf("session-controller: merge resolution turn NOT STARTED (workspace busy) ws=%q session=%s request_id=%s entry=%s: %v — the merge is failed now rather than competing with the user's turn for the shim",
			workspace, d.sessionID, requestID, disposition.queuedEntryID, err)
		return err
	}
	bind := time.NewTimer(m.mergeResolutionBindBound())
	defer bind.Stop()
	select {
	case <-w.boundDone:
	case <-bind.C:
		err := fmt.Errorf("session-controller: the merge resolution turn for workspace %q (request %s) never started: no turn began within %s of the submit, and %s",
			workspace, requestID, m.mergeResolutionBindBound(), disposition)
		m.logf("session-controller: merge resolution turn NEVER STARTED ws=%q session=%s request_id=%s bind_bound=%s: %v — the merge is failed now rather than holding the shim for the rest of the turn bound",
			workspace, d.sessionID, requestID, m.mergeResolutionBindBound(), err)
		return err
	case <-ctx.Done():
		err := fmt.Errorf("session-controller: the merge resolution turn for workspace %q (request %s) never started: %w, and %s",
			workspace, requestID, ctx.Err(), disposition)
		m.logf("session-controller: merge resolution turn NEVER STARTED ws=%q session=%s request_id=%s: %v — the cherry-pick is NOT resumed",
			workspace, d.sessionID, requestID, err)
		return err
	}
	select {
	case <-w.done:
		// THE TURN'S OWN VERDICT, not merely that it ended. A turn that ended
		// badly is a merge action that did not happen, and reporting it as
		// complete is how an errored before-action came to gate nothing and an
		// errored after-action came to reach no frontend.
		if outcome := m.turnWaiterOutcome(w); outcome.isError {
			err := fmt.Errorf("session-controller: the merge turn for workspace %q (request %s) ENDED IN ERROR (stop_reason=%q)",
				workspace, requestID, outcome.stopReason)
			m.logf("session-controller: merge resolution turn ERRORED ws=%q session=%s request_id=%s turn_id=%q stop_reason=%q — the caller is told the action did not happen",
				workspace, d.sessionID, requestID, m.turnWaiterID(w), outcome.stopReason)
			return err
		}
		m.logf("session-controller: merge resolution turn ENDED ws=%q session=%s request_id=%s turn_id=%q",
			workspace, d.sessionID, requestID, m.turnWaiterID(w))
		return nil
	case <-ctx.Done():
		err := fmt.Errorf("session-controller: the merge resolution turn for workspace %q (request %s) never reported its end: %w",
			workspace, requestID, ctx.Err())
		boundTurnID := m.turnWaiterID(w)
		m.logf("session-controller: merge resolution turn UNFINISHED ws=%q session=%s request_id=%s bound_turn_id=%q: %v — the cherry-pick is NOT resumed",
			workspace, d.sessionID, requestID, boundTurnID, err)
		// THE ONLY EXIT THAT LEAVES A RUNNING TURN BEHIND, and therefore the one
		// that has to stop it. The run is about to reach a terminal and release
		// the lease, so this turn's output has no consumer and the workspace is
		// handed back to a user who would be fighting it for their own shim.
		// See mergeturnstop.go for why the stop is aimed at this ONE turn id.
		m.stopAbandonedMergeTurn(d, workspace, requestID, boundTurnID)
		return err
	}
}

// turnWaiterID reads w's bound turn id under the manager mutex, for logging.
func (m *Manager) turnWaiterID(w *turnWaiter) string {
	m.mu.Lock()
	defer m.mu.Unlock()
	return w.turnID
}

// turnWaiterOutcome reads w's completed outcome under the manager mutex. Only
// called after w.done is closed, which is when the field is final.
func (m *Manager) turnWaiterOutcome(w *turnWaiter) turnOutcome {
	m.mu.Lock()
	defer m.mu.Unlock()
	return w.outcome
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

	// The brief is read from prompts/ per use, so it can fail. A conflict whose
	// instruction cannot be composed is left PARKED for a human, exactly as a
	// refused submit would leave it — never resolved by an agent working from a
	// half-formed brief.
	prompt, err := res.Prompt()
	if err != nil {
		m.logf("session-controller: merge conflict resolution FAILED ws=%q request_id=%s commit=%s — prompt unavailable: %v",
			res.Workspace, res.RequestID, res.ConflictCommit, err)
		return err
	}

	ctx, cancel := context.WithTimeout(ctx, m.mergeResolutionBound())
	defer cancel()
	if err := m.SubmitMergePromptAwaitingTurn(ctx, res.Workspace, res.RequestID, prompt, mergeResolutionPermissionMode, corev1.PromptOrigin_PROMPT_ORIGIN_MERGE_CONFLICT_REPAIR); err != nil {
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

	// As on the conflict path: an uncomposable brief fails the fix attempt
	// outright rather than spending the single permitted attempt on a guess.
	prompt, err := res.Prompt()
	if err != nil {
		m.logf("session-controller: merge test-failure resolution FAILED ws=%q request_id=%s commit=%s — prompt unavailable: %v",
			res.Workspace, res.RequestID, res.FailingCommit, err)
		return err
	}

	ctx, cancel := context.WithTimeout(ctx, m.mergeResolutionBound())
	defer cancel()
	if err := m.SubmitMergePromptAwaitingTurn(ctx, res.Workspace, res.RequestID, prompt, mergeResolutionPermissionMode, corev1.PromptOrigin_PROMPT_ORIGIN_MERGE_TEST_REPAIR); err != nil {
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
	if err := m.SubmitMergePromptAwaitingTurn(ctx, act.Workspace, act.RequestID, act.Prompt, mergeResolutionPermissionMode, corev1.PromptOrigin_PROMPT_ORIGIN_MERGE_BEFORE_ACTION); err != nil {
		m.logf("session-controller: merge before-action FAILED ws=%q request_id=%s: %v", act.Workspace, act.RequestID, err)
		return err
	}
	m.logf("session-controller: merge before-action turn COMPLETE ws=%q request_id=%s — merge.Coordinator computes the cherry-pick plan now",
		act.Workspace, act.RequestID)
	return nil
}

// RunMergeAfterAction implements merge.AfterActionRunner (through the server's
// PromptRouter): it submits the workspace's recorded postprocessing action on
// the merge lease holder's behalf and returns once that turn has ended.
//
// It is the fourth sibling, and it differs from the before-action in exactly one
// place — and NOT here. Its turn is submitted, awaited and judged identically;
// what merge.Coordinator does with a failure is the difference, and that
// decision belongs there (the commits are already on the target, so the error
// rides on the terminal merged status rather than failing the run). Softening
// the verdict here would leave that status carrying nothing at all.
func (m *Manager) RunMergeAfterAction(ctx context.Context, act merge.AfterAction) error {
	if act.Workspace == "" {
		return fmt.Errorf("session-controller: a merge after-action needs a workspace")
	}
	if act.RequestID == "" {
		return fmt.Errorf("session-controller: a merge after-action for workspace %q needs a request id", act.Workspace)
	}
	if act.Prompt == "" {
		return fmt.Errorf("session-controller: a merge after-action for workspace %q needs a prompt", act.Workspace)
	}
	m.logf("session-controller: merge after-action ws=%q request_id=%s — driving the workspace's own session under the merge lease now that every commit has landed",
		act.Workspace, act.RequestID)

	ctx, cancel := context.WithTimeout(ctx, m.mergeResolutionBound())
	defer cancel()
	if err := m.SubmitMergePromptAwaitingTurn(ctx, act.Workspace, act.RequestID, act.Prompt, mergeResolutionPermissionMode, corev1.PromptOrigin_PROMPT_ORIGIN_MERGE_AFTER_ACTION); err != nil {
		m.logf("session-controller: merge after-action FAILED ws=%q request_id=%s: %v — the merge STANDS; merge.Coordinator carries this onto the terminal merged status",
			act.Workspace, act.RequestID, err)
		return err
	}
	m.logf("session-controller: merge after-action turn COMPLETE ws=%q request_id=%s", act.Workspace, act.RequestID)
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

// mergeResolutionBindBound resolves the BIND phase's bound the same way: the
// configured override when a harness set one, the package default otherwise.
func (m *Manager) mergeResolutionBindBound() time.Duration {
	if m.cfg.MergeResolutionTurnBindBound > 0 {
		return m.cfg.MergeResolutionTurnBindBound
	}
	return mergeResolutionTurnBindBound
}
