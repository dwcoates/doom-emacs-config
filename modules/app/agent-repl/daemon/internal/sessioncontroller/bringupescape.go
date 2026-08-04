// bringupescape.go closes the loop `starting` used to be able to sit in
// forever.
//
// THE DEFECT. A record naming a vendor conversation with no transcript made
// every bring-up run `claude --resume <uuid>`; the CLI exited 1, the shim shut
// down with `sdk_error`, and the connection died before ShimReady. Nothing
// closed the legacy connectivity projection — the axis has an opening edge at ShimReady and close
// edges for hibernation and controller exit, and a shim that dies mid-handshake
// while the client loops waiting to reconnect is none of those — so the
// workspace stayed blue-on-`starting` with no card, no error, and no end.
//
// THE LADDER, in order, each rung bounded:
//
//  1. A bring-up that neither wires nor faults within bringUpTimeout is a
//     failure, not a wait. (Already true; what is new is that it now RESOLVES.)
//  2. A transport failure preserves the durable resume pointer and retries the
//     same conversation exactly once.
//  3. A vendor failure or failed retry is terminal: the axis closes with
//     `bring_up_failed` and a loud failure card names the error.
//
// So every bring-up now ends on `wired` or on a resolved failure, and
// "wedged on starting" has no path left.
package sessioncontroller

import (
	"context"
	"errors"
	"fmt"

	corev1 "agentrepl/proto/agentshim/core/v1"

	"claude-repld/internal/errclass"
	"claude-repld/internal/ssm"
)

// shimSDKComponent is the component a claude-shim names when its SDK stream
// fails — the ONE thing the daemon learns about WHY a shim died during
// bring-up. The shim sends this DegradedState immediately before shutting down
// with `sdk_error`, so it arrives over the live connection even when ShimReady
// never does, and it carries the vendor's own message verbatim.
const shimSDKComponent = "claude-shim-sdk"

type bringUpFailureKind string

const (
	bringUpFailureTransport bringUpFailureKind = "transport"
	bringUpFailureTimeout   bringUpFailureKind = "timeout"
	bringUpFailureVendor    bringUpFailureKind = "vendor"
	bringUpFailureCanceled  bringUpFailureKind = "canceled"
)

type bringUpFailure struct {
	kind  bringUpFailureKind
	cause error
}

func (e *bringUpFailure) Error() string { return e.cause.Error() }
func (e *bringUpFailure) Unwrap() error { return e.cause }

// noteBringUpFault records a shim-reported SDK failure observed while a
// bring-up is still waiting, and WAKES the wait.
//
// Without it the wait runs the full bringUpTimeout for a shim that is already
// gone, which is correct but slow — and the UX contract asks for a brief retry,
// not a half-minute one. The reason is kept because it is the only account of
// the failure the daemon will ever hold: the shim's exit carries no reason on
// the wire, and this report precedes it.
func (m *Manager) noteBringUpFault(d *sessionController, ds *corev1.DegradedState) {
	if ds.GetComponent() != shimSDKComponent || ds.GetRecovered() {
		return
	}
	m.mu.Lock()
	if d.wired || d.faultReason != "" {
		m.mu.Unlock()
		return
	}
	d.faultReason = ds.GetReason()
	m.mu.Unlock()
	m.logf("session-controller: BRING-UP FAULT ws=%q session=%s — the shim reported its SDK stream failed before the connection was ready: %s",
		d.workspace, d.sessionID, ds.GetReason())
	close(d.faulted)
}

// noteWired marks the bring-up gate as CLOSED for the workspace's controller, so a
// later SDK failure is an ordinary mid-session degradation rather than a
// bring-up fault. Called from the ShimReady hook, which is the gate itself, so
// it holds even for the bring-ups nothing is waiting on (Ensure).
func (m *Manager) noteWired(workspace, sessionID string) {
	m.mu.Lock()
	if d, ok := m.byWS[workspace]; ok && d.sessionID == sessionID {
		d.wired = true
	}
	m.mu.Unlock()
}

// awaitDriveable waits for the bring-up to finish, one way or the other.
//
// Three outcomes, all bounded: the handshake lands (nil), the shim reports its
// SDK died (the fault), or bringUpTimeout elapses. There is no fourth, which is
// what makes `starting` non-terminal.
func (m *Manager) awaitDriveable(ctx context.Context, d *sessionController) error {
	waitCtx, cancel := context.WithTimeout(ctx, bringUpTimeout)
	defer cancel()
	ready := make(chan error, 1)
	go func() { ready <- d.client.AwaitReady(waitCtx) }()
	select {
	case err := <-ready:
		if err != nil {
			kind := bringUpFailureTransport
			switch {
			case ctx.Err() != nil:
				kind = bringUpFailureCanceled
			case errors.Is(err, context.DeadlineExceeded):
				kind = bringUpFailureTimeout
			}
			return &bringUpFailure{kind: kind, cause: fmt.Errorf("session-controller: session %s for workspace %q never became driveable: %w", d.sessionID, d.workspace, err)}
		}
		m.noteWired(d.workspace, d.sessionID)
		return nil
	case <-d.faulted:
		m.mu.Lock()
		reason := d.faultReason
		m.mu.Unlock()
		return &bringUpFailure{kind: bringUpFailureVendor, cause: fmt.Errorf("session-controller: session %s for workspace %q died during bring-up: %s", d.sessionID, d.workspace, reason)}
	}
}

// escapeFailedBringUp is rungs 2 and 3 of the ladder. It always resolves: it
// either returns a session controller that IS wired (the same-conversation
// transport retry worked) or a loud error whose failure card and closed axis
// have already been published.
func (m *Manager) escapeFailedBringUp(ctx context.Context, workspace string, d *sessionController, cause error) (*sessionController, error) {
	// A retired generation has no authority to stop the shim or rewrite durable
	// conversation identity. Re-enter ensure through the current generation so
	// the original waiter observes its replacement instead of sabotaging it.
	m.mu.Lock()
	current := m.byWS[workspace]
	m.mu.Unlock()
	if current != d {
		m.cancelRetiredBringUp(workspace, d, current, "pre_teardown", cause)
		if current == nil {
			return nil, cause
		}
		return m.ensure(ctx, workspace)
	}

	if !m.tearDownFailedBringUp(workspace, d) {
		m.mu.Lock()
		current = m.byWS[workspace]
		m.mu.Unlock()
		m.cancelRetiredBringUp(workspace, d, current, "teardown_race", cause)
		if current == nil {
			return nil, cause
		}
		return m.ensure(ctx, workspace)
	}

	kind := bringUpFailureTransport
	var classified *bringUpFailure
	if errors.As(cause, &classified) {
		kind = classified.kind
	}
	if kind != bringUpFailureTransport && kind != bringUpFailureTimeout {
		m.logf("session-controller: bring-up FAILED ws=%q session=%s generation=%s failure_kind=%s resume=%q resume_decision=preserve retry=false cause=%v",
			workspace, d.sessionID, d.generationID, kind, d.resumedVendorSessionID, cause)
		m.resolveStartFailed(workspace, d, cause)
		return nil, cause
	}

	m.logf("session-controller: bring-up FAILED ws=%q session=%s generation=%s failure_kind=%s resume=%q resume_decision=preserve retry=same_conversation_once cause=%v",
		workspace, d.sessionID, d.generationID, kind, d.resumedVendorSessionID, cause)

	retry, created, err := m.bringUpTracked(workspace)
	if err != nil {
		m.resolveStartFailed(workspace, d, err)
		return nil, err
	}
	if !created {
		m.logf("session-controller: same-conversation retry lost concurrent bring-up race ws=%q failed_session=%s failed_generation=%s current_session=%s current_generation=%s decision=observe_current resume_decision=preserve",
			workspace, d.sessionID, d.generationID, retry.sessionID, retry.generationID)
		return m.ensure(ctx, workspace)
	}
	if d.resumedVendorSessionID != "" && retry.resumedVendorSessionID != d.resumedVendorSessionID {
		err := fmt.Errorf("session-controller: transport retry changed resume identity for session %s from %q to %q",
			d.sessionID, d.resumedVendorSessionID, retry.resumedVendorSessionID)
		m.logf("session-controller: bring-up retry invariant FAILED ws=%q session=%s first_generation=%s retry_generation=%s resume_decision=abort expected_resume=%q actual_resume=%q cause=%v",
			workspace, d.sessionID, d.generationID, retry.generationID, d.resumedVendorSessionID, retry.resumedVendorSessionID, err)
		m.tearDownFailedBringUp(workspace, retry)
		m.resolveStartFailed(workspace, retry, err)
		return nil, err
	}
	if err := m.awaitDriveable(ctx, retry); err != nil {
		m.tearDownFailedBringUp(workspace, retry)
		m.logf("session-controller: same-conversation transport retry failed ws=%q session=%s generation=%s resume=%q resume_decision=preserve retry=false cause=%v",
			workspace, retry.sessionID, retry.generationID, retry.resumedVendorSessionID, err)
		m.resolveStartFailed(workspace, retry, err)
		return nil, err
	}
	return retry, nil
}

// tearDownFailedBringUp evicts a session controller that never wired and stops the shim it
// spawned, so the retry starts from nothing rather than racing a corpse.
func (m *Manager) tearDownFailedBringUp(workspace string, d *sessionController) bool {
	m.mu.Lock()
	wasCurrent := false
	if cur, ok := m.byWS[workspace]; ok && cur == d {
		delete(m.byWS, workspace)
		wasCurrent = true
	}
	m.mu.Unlock()
	if !wasCurrent {
		if d.cancel != nil {
			d.cancel()
		}
		return false
	}
	// The drain runs before the cancel like every other teardown's, even though
	// a bring-up that never wired has almost never started a turn: a RETRY
	// after a partially handshaked shim is the exception, and the interrupt
	// costs nothing when the connection was never established.
	m.drainLiveTurnForStop(workspace, d.sessionID, StopCauseBringUpFailed().path(), d.client)
	if d.cancel != nil {
		d.cancel()
	}
	// SOLE SESSION CONTROLLER ONLY WHEN THIS SESSION CONTROLLER OWNED THE WORKSPACE. A bring-up that
	// lost the concurrent-first-prompt race is being torn down while the winner
	// drives the workspace, and closing an unattributed claim there would blue
	// out the winner's live turn.
	if err := m.stopShimSettlingTurn(workspace, d.sessionID, StopCauseBringUpFailed(), wasCurrent); err != nil {
		m.logf("session-controller: stopping the shim of a failed bring-up FAILED ws=%q session=%s: %v", workspace, d.sessionID, err)
	}
	return true
}

// cancelRetiredBringUp releases only the retired controller's private client.
// It never stops a shim or publishes durable/render state because current is
// the sole owner of those effects, including when current is nil after an
// explicit hibernation removed the workspace.
func (m *Manager) cancelRetiredBringUp(workspace string, d, current *sessionController, phase string, cause error) {
	currentGeneration := ""
	currentSession := ""
	decision := "return_failure"
	if current != nil {
		currentGeneration = current.generationID
		currentSession = current.sessionID
		decision = "observe_current"
	}
	m.logf("session-controller: bring-up failure belongs to retired generation ws=%q session=%s failed_generation=%s current_session=%s current_generation=%s phase=%s decision=%s stop_shim=false resume_decision=preserve cause=%v",
		workspace, d.sessionID, d.generationID, currentSession, currentGeneration, phase, decision, cause)
	if d.cancel != nil {
		d.cancel()
	}
}

// resolveStartFailed is the close edge `starting` never had: the axis reports
// the wiring GONE and the feed gets a card naming the error.
//
// The axis closes to `severed` rather than to a token of its own, and severed
// rather than hibernated is the load-bearing half of that. A bring-up we asked
// for and could not complete is EVIDENCE THAT SOMETHING BROKE, which is the one
// thing that separates the two closed halves of the axis: teal would claim this
// workspace is merely asleep, when in fact a spawn or a handshake failed and the
// user is owed a blue tab. The CARD is where the finer distinction between
// "nothing was asked for" and "something was asked for and could not be started"
// lives — the same division of labour the color vocabulary already makes for
// every other local fault.
//
// This is also why the session-controller-exit tail can stay silent on a clean exit: the
// teardown here cancels the session controller ctx, ending Run with nil, and this row is
// already the truer answer that tail would otherwise overwrite.
func (m *Manager) resolveStartFailed(workspace string, d *sessionController, cause error) {
	m.logf("session-controller: START FAILED ws=%q session=%s generation=%s connectivity=unavailable cause=%v branch=bring_up_failed",
		workspace, d.sessionID, d.generationID, cause)
	if d.consumer != nil {
		d.consumer.pushFailure(d.consumer.startFailedUUID(), errclass.StartFailed(cause.Error()))
	}
	m.noteConnectivity(workspace, d.sessionID, d.generationID, ssm.SessionConnectivityUnavailable, "bring_up_failed")
}
