// bringupescape.go closes the loop `starting` used to be able to sit in
// forever.
//
// THE DEFECT. A record naming a vendor conversation with no transcript made
// every bring-up run `claude --resume <uuid>`; the CLI exited 1, the shim shut
// down with `sdk_error`, and the connection died before ShimReady. Nothing
// closed the WIRED axis — the axis has an opening edge at ShimReady and close
// edges for hibernation and driver exit, and a shim that dies mid-handshake
// while the client loops waiting to reconnect is none of those — so the
// workspace stayed blue-on-`starting` with no card, no error, and no end.
//
// THE LADDER, in order, each rung bounded:
//
//  1. A bring-up that neither wires nor faults within bringUpTimeout is a
//     failure, not a wait. (Already true; what is new is that it now RESOLVES.)
//  2. A bring-up that failed WITH a resume pointer is classified as a bad
//     pointer: drop it, note it in the feed, retry FRESH exactly once.
//  3. A fresh bring-up that fails is terminal: the axis closes with
//     `bring_up_failed` and a loud failure card names the error.
//
// So every bring-up now ends on `wired` or on a resolved failure, and
// "wedged on starting" has no path left.
package sessiondrv

import (
	"context"
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

// noteBringUpFault records a shim-reported SDK failure observed while a
// bring-up is still waiting, and WAKES the wait.
//
// Without it the wait runs the full bringUpTimeout for a shim that is already
// gone, which is correct but slow — and the UX contract asks for a brief retry,
// not a half-minute one. The reason is kept because it is the only account of
// the failure the daemon will ever hold: the shim's exit carries no reason on
// the wire, and this report precedes it.
func (m *Manager) noteBringUpFault(d *driven, ds *corev1.DegradedState) {
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
	m.logf("sessiondrv: BRING-UP FAULT ws=%q session=%s — the shim reported its SDK stream failed before the connection was ready: %s",
		d.workspace, d.sessionID, ds.GetReason())
	close(d.faulted)
}

// noteWired marks the bring-up gate as CLOSED for the workspace's driver, so a
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
func (m *Manager) awaitDriveable(ctx context.Context, d *driven) error {
	waitCtx, cancel := context.WithTimeout(ctx, bringUpTimeout)
	defer cancel()
	ready := make(chan error, 1)
	go func() { ready <- d.client.AwaitReady(waitCtx) }()
	select {
	case err := <-ready:
		if err != nil {
			return fmt.Errorf("sessiondrv: session %s for workspace %q never became driveable: %w", d.sessionID, d.workspace, err)
		}
		m.noteWired(d.workspace, d.sessionID)
		return nil
	case <-d.faulted:
		m.mu.Lock()
		reason := d.faultReason
		m.mu.Unlock()
		return fmt.Errorf("sessiondrv: session %s for workspace %q died during bring-up: %s", d.sessionID, d.workspace, reason)
	}
}

// escapeFailedBringUp is rungs 2 and 3 of the ladder. It always resolves: it
// either returns a driver that IS wired (the fresh retry worked) or a loud
// error whose failure card and closed axis have already been published.
func (m *Manager) escapeFailedBringUp(ctx context.Context, workspace string, d *driven, cause error) (*driven, error) {
	m.tearDownFailedBringUp(workspace, d)

	dropped, dropErr := m.cfg.Spawner.DropResume(d.sessionID)
	if dropErr != nil {
		// The pointer could not be cleared, so a retry would resume the very
		// thing that just failed. Resolve rather than loop.
		m.resolveStartFailed(workspace, d, fmt.Errorf("%w (and the stale resume pointer could not be dropped: %v)", cause, dropErr))
		return nil, cause
	}
	if dropped == "" {
		// The bring-up was already FRESH. There is nothing left to try, and
		// retrying the identical spawn would only spend the user's time.
		m.logf("sessiondrv: bring-up FAILED ws=%q session=%s with no resume pointer to drop — resolving start-failed: %v",
			workspace, d.sessionID, cause)
		m.resolveStartFailed(workspace, d, cause)
		return nil, cause
	}

	m.logf("sessiondrv: bring-up FAILED ws=%q session=%s while resuming vendor conversation %s — dropping the pointer and retrying FRESH exactly once: %v",
		workspace, d.sessionID, dropped, cause)
	m.pushHistoryMissingNote(d, dropped, cause.Error())

	retry, err := m.bringUp(workspace)
	if err != nil {
		m.resolveStartFailed(workspace, d, err)
		return nil, err
	}
	if err := m.awaitDriveable(ctx, retry); err != nil {
		m.tearDownFailedBringUp(workspace, retry)
		m.logf("sessiondrv: the FRESH retry failed too ws=%q session=%s — resolving start-failed: %v", workspace, retry.sessionID, err)
		m.resolveStartFailed(workspace, retry, err)
		return nil, err
	}
	return retry, nil
}

// tearDownFailedBringUp evicts a driver that never wired and stops the shim it
// spawned, so the retry starts from nothing rather than racing a corpse.
func (m *Manager) tearDownFailedBringUp(workspace string, d *driven) {
	m.mu.Lock()
	wasCurrent := false
	if cur, ok := m.byWS[workspace]; ok && cur == d {
		delete(m.byWS, workspace)
		wasCurrent = true
	}
	m.mu.Unlock()
	// The drain runs before the cancel like every other teardown's, even though
	// a bring-up that never wired has almost never started a turn: a RETRY
	// after a partially handshaked shim is the exception, and the interrupt
	// costs nothing when the connection was never established.
	m.drainLiveTurnForStop(workspace, d.sessionID, "bringup_failed", d.client)
	if d.cancel != nil {
		d.cancel()
	}
	// SOLE DRIVER ONLY WHEN THIS DRIVER OWNED THE WORKSPACE. A bring-up that
	// lost the concurrent-first-prompt race is being torn down while the winner
	// drives the workspace, and closing an unattributed claim there would blue
	// out the winner's live turn.
	if err := m.stopShimSettlingTurn(workspace, d.sessionID, "bringup_failed", wasCurrent); err != nil {
		m.logf("sessiondrv: stopping the shim of a failed bring-up FAILED ws=%q session=%s: %v", workspace, d.sessionID, err)
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
// This is also why the driver-exit tail can stay silent on a clean exit: the
// teardown here cancels the driver ctx, ending Run with nil, and this row is
// already the truer answer that tail would otherwise overwrite.
func (m *Manager) resolveStartFailed(workspace string, d *driven, cause error) {
	m.logf("sessiondrv: START FAILED ws=%q session=%s — the wiring is resolved rather than left in `starting`: %v",
		workspace, d.sessionID, cause)
	if d.consumer != nil {
		d.consumer.pushFailure(d.consumer.startFailedUUID(), errclass.StartFailed(cause.Error()))
	}
	m.noteWiring(workspace, ssm.WiringSevered, "bring_up_failed")
}

// pushHistoryMissingNote tells the user, in their own feed, that the
// conversation they expected to continue is not coming back.
//
// It is the whole repair as far as they are concerned: the session comes up
// green either way, and without the note a silently emptied workspace is
// indistinguishable from lost data.
func (m *Manager) pushHistoryMissingNote(d *driven, droppedUUID, detail string) {
	if d.consumer == nil || droppedUUID == "" {
		return
	}
	d.consumer.pushFailure(d.consumer.historyMissingUUID(droppedUUID), errclass.HistoryMissing(droppedUUID, detail))
}
