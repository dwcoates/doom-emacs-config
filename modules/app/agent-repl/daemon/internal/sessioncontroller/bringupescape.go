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
//  4. A session whose bring-ups keep failing is GIVEN UP ON after
//     bringUpGiveUpAfter of them in a row: the ladder stops respawning and the
//     session parks on the same closed axis and failure card rung 3 publishes,
//     with the card naming the give-up.
//
// So every bring-up ends on `wired` or on a resolved failure, and "wedged on
// starting" has no path left.
//
// RUNG 4 IS A BOUND ON THE LADDER, NOT A REPLACEMENT FOR IT. Every rung above
// still runs exactly as it did; the bound only decides when the ladder stops
// being re-climbed. Without it a session that cannot come up is retried
// forever, and each attempt costs a full bringUpTimeout during which the
// daemon's inline frontend read loop dispatches nothing.
package sessioncontroller

import (
	"context"
	"errors"
	"fmt"
	"time"

	corev1 "agentrepl/proto/agentshim/core/v1"

	"claude-repld/internal/errclass"
	"claude-repld/internal/ssm"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// shimSDKComponent is the component a claude-shim names when its SDK stream
// fails — the ONE thing the daemon learns about WHY a shim died during
// bring-up. The shim sends this DegradedState immediately before shutting down
// with `sdk_error`, so it arrives over the live connection even when ShimReady
// never does, and it carries the vendor's own message verbatim.
const shimSDKComponent = "claude-shim-sdk"

// bringUpGiveUpAfter is how many CONSECUTIVE resolved bring-up failures a
// session is allowed before the daemon stops respawning it.
//
// Three, because a healthy bring-up takes under four seconds end to end while a
// failing one costs a full bringUpTimeout: three is enough to ride out a
// transient (a machine under load, a vendor blip) and few enough that a session
// which genuinely cannot start stops eating half a minute of the daemon's
// attention per attempt. The count is per session and is cleared by any
// successful bring-up, so an intermittent session never accumulates toward it.
const bringUpGiveUpAfter = 3

// THE PARK IS A COOLDOWN, NOT A WALL.
//
// The bound above stops the daemon respawning a session that cannot start. What
// it must NOT do is dead-end the workspace, and as first written it did exactly
// that: the only two edges that cleared the streak were a bring-up that WIRED
// and a deliberate hibernation — and a parked session is refused BEFORE
// anything is spawned, so the wiring that would clear it could never happen.
// The one clearing edge reachable from a park was therefore an action no
// message ever named. Every subsequent open of that workspace was nacked, for
// as long as the daemon lived.
//
// So reaching the bound now starts a COOLDOWN. While it runs, bring-up is
// refused exactly as before, and the refusal says how long is left. When it
// expires, the next bring-up request RELEASES the park and climbs the ladder
// once more; a failure re-parks immediately on a longer cooldown, so a session
// that genuinely cannot start costs one attempt per cooldown rather than a
// tight crash-loop. The bound's whole purpose — never burn the daemon's
// attention on an endless respawn — is preserved; only the permanence is gone.
const (
	// bringUpParkCooldown is the first park's duration. One wasted attempt per
	// minute is a rounding error against the daemon's attention, and a minute
	// is short enough that a user who steps away and comes back finds a
	// workspace that will try again rather than one that has given up forever.
	bringUpParkCooldown = time.Minute
	// bringUpParkCooldownMax caps the doubling. A session parked this long is
	// broken in a way no retry cadence fixes, and the cap keeps the workspace
	// self-healing on the timescale a user might plausibly return on.
	bringUpParkCooldownMax = 15 * time.Minute
)

// ErrBringUpGaveUp reports that a session reached bringUpGiveUpAfter
// consecutive bring-up failures and is parked on its cooldown.
//
// It is a sentinel because it is a POLICY refusal rather than a failure of the
// attempt in front of the caller: nothing was tried this time. A caller must be
// able to tell "this session is parked" from "this bring-up failed" without
// matching message text.
var ErrBringUpGaveUp = errors.New("session-controller: the session has failed bring-up too many times in a row and is not being respawned yet")

// bringUpStreak is one session's consecutive-failure count plus, once the bound
// is reached, the cooldown that park is serving.
type bringUpStreak struct {
	// failures is the number of CONSECUTIVE resolved bring-up failures.
	failures int
	// parkedUntilMs is when the current park expires, on the Manager's clock.
	// Zero means not parked.
	parkedUntilMs int64
	// cooldown is the park duration in force, doubled on each re-park and
	// capped at bringUpParkCooldownMax.
	cooldown time.Duration
}

// noteBringUpFailure records one resolved bring-up failure and reports the
// session's consecutive-failure count together with whether that count has
// reached the give-up bound. Reaching it (re-)arms the park's cooldown.
func (m *Manager) noteBringUpFailure(sessionID string) (failures int, givenUp bool, cooldown time.Duration) {
	m.mu.Lock()
	defer m.mu.Unlock()
	if m.bringUpFailures == nil {
		m.bringUpFailures = make(map[string]*bringUpStreak)
	}
	s := m.bringUpFailures[sessionID]
	if s == nil {
		s = &bringUpStreak{}
		m.bringUpFailures[sessionID] = s
	}
	s.failures++
	if s.failures < bringUpGiveUpAfter {
		return s.failures, false, 0
	}
	if s.cooldown == 0 {
		s.cooldown = bringUpParkCooldown
	} else if s.cooldown < bringUpParkCooldownMax {
		s.cooldown *= 2
		if s.cooldown > bringUpParkCooldownMax {
			s.cooldown = bringUpParkCooldownMax
		}
	}
	s.parkedUntilMs = m.now() + s.cooldown.Milliseconds()
	return s.failures, true, s.cooldown
}

// bringUpFailuresFor reports the session's consecutive resolved bring-up
// failures so far, NOT counting one currently being resolved.
func (m *Manager) bringUpFailuresFor(sessionID string) int {
	m.mu.Lock()
	defer m.mu.Unlock()
	if s := m.bringUpFailures[sessionID]; s != nil {
		return s.failures
	}
	return 0
}

// bringUpParkRemaining reports how much of the session's park is left, and
// RELEASES an expired park as a side effect so the caller's bring-up proceeds.
//
// A release drops the failure count to one below the bound rather than to zero:
// the session has not proven anything yet, so a single further failure re-parks
// it at once. That is what keeps the released attempt an attempt rather than
// the start of a fresh streak of bringUpGiveUpAfter timeouts.
func (m *Manager) bringUpParkRemaining(sessionID string) (remaining time.Duration, parked bool) {
	m.mu.Lock()
	s := m.bringUpFailures[sessionID]
	if s == nil || s.failures < bringUpGiveUpAfter {
		m.mu.Unlock()
		return 0, false
	}
	if leftMs := s.parkedUntilMs - m.now(); leftMs > 0 {
		left := time.Duration(leftMs) * time.Millisecond
		cooldown, failures := s.cooldown, s.failures
		m.mu.Unlock()
		m.logf("session-controller: bring-up park HOLDS session=%s failures=%d bound=%d cooldown=%s remaining=%s — the cooldown has not expired, so no shim is spawned",
			sessionID, failures, bringUpGiveUpAfter, cooldown, left.Round(time.Second))
		return left, true
	}
	served := s.cooldown
	s.failures = bringUpGiveUpAfter - 1
	s.parkedUntilMs = 0
	m.mu.Unlock()
	m.logf("session-controller: bring-up park RELEASED session=%s cooldown_served=%s failures=%d bound=%d — the cooldown expired, so this bring-up climbs the ladder again",
		sessionID, served, bringUpGiveUpAfter-1, bringUpGiveUpAfter)
	return 0, false
}

// clearBringUpFailures resets the session's consecutive-failure count AND its
// park, cooldown included. Called from the edges that make the streak
// meaningless: a bring-up that WIRED, a deliberate hibernation, and a hard
// restart — the last being the user's explicit "replace this process", which is
// the immediate way out of a park that has not yet cooled down.
func (m *Manager) clearBringUpFailures(sessionID string) {
	m.mu.Lock()
	s := m.bringUpFailures[sessionID]
	delete(m.bringUpFailures, sessionID)
	m.mu.Unlock()
	if s != nil && s.failures > 0 {
		m.logf("session-controller: bring-up failure streak CLEARED session=%s failures=%d cooldown=%s", sessionID, s.failures, s.cooldown)
	}
}

type bringUpFailureKind string

const (
	bringUpFailureTransport bringUpFailureKind = "transport"
	bringUpFailureTimeout   bringUpFailureKind = "timeout"
	bringUpFailureVendor    bringUpFailureKind = "vendor"
	bringUpFailureCanceled  bringUpFailureKind = "canceled"
)

type bringUpFailure struct {
	kind             bringUpFailureKind
	cause            error
	queryTermination *frontendv1.QueryTerminationFailure
}

func (e *bringUpFailure) Error() string { return e.cause.Error() }
func (e *bringUpFailure) Unwrap() error { return e.cause }

// QueryTerminationFailureDetail returns the typed SDK termination that caused
// a pre-readiness bring-up failure. A nil result means the failure did not
// arrive through the typed query-lifecycle path.
func (e *bringUpFailure) QueryTerminationFailureDetail() *frontendv1.QueryTerminationFailure {
	return e.queryTermination
}

// noteBringUpTermination retains the typed lifecycle record that must precede
// the paired degraded wake-up. The pair is delivered on one consumer read loop,
// so the gate cannot wake before the exact termination is retained.
func (m *Manager) noteBringUpTermination(d *sessionController, detail *frontendv1.QueryTerminationFailure) {
	if detail == nil {
		panic("session-controller: nil typed query termination reached bring-up gate")
	}
	m.mu.Lock()
	defer m.mu.Unlock()
	if d.wired || d.faultTermination != nil {
		return
	}
	d.faultTermination = detail
	m.logf("session-controller: BRING-UP QUERY TERMINATION retained ws=%q session=%s query_instance_id=%q vendor_session_id=%q vendor_identity_unavailable=%t observed_at_ms=%d",
		d.workspace, d.sessionID, detail.GetQueryInstanceId(), detail.GetVendorSessionId(), detail.GetVendorSessionIdentityUnavailable() != nil, detail.GetObservedAtMs())
}

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
	// A bring-up that wired says the previous failures were transient, so the
	// streak toward the give-up bound starts again from zero.
	m.clearBringUpFailures(sessionID)
	// And the owed automatic retry with it (bringupretry.go): the workspace is
	// wired, so there is nothing left for the sweep to climb.
	m.clearBringUpRetry(workspace, "wired")
	// THE GATE LEDGER IS CLOSED OUT BY THE REWIRE ITSELF (hibernation.go). A
	// wired shim and a record claiming a sleep is a contradiction, and it is
	// retired HERE rather than by each path that happens to wire a sleeping
	// session, so no future one can forget. It runs BEFORE the owed-resumption
	// drive below, because a resumption submitted against a standing gate would
	// be nacked by the very record this retires.
	m.closeHibernationGateOnWire(workspace, sessionID)
	// THE LEVEL-TRIGGER FOR AN OWED RESUMPTION (turnresumption.go). This is the
	// ONE point at which a session becomes driveable — every bring-up reaches
	// it, a fresh spawn and a reattach alike — so it is where the store is asked
	// what the last bounce interrupted. On its own goroutine because a bring-up
	// must never block on a submit.
	go m.driveOwedResumptions(workspace, sessionID)
}

// awaitDriveable waits for the bring-up to finish, one way or the other.
//
// Three outcomes, all bounded: the handshake lands (nil), the shim reports its
// SDK died (the fault), or bringUpTimeout elapses. There is no fourth, which is
// what makes `starting` non-terminal.
func (m *Manager) awaitDriveable(ctx context.Context, d *sessionController) error {
	// The CAP, not the failure bound: AwaitReady resolves a wedged shim on its
	// own silence window (bringUpTimeout). This only stops an attempt that keeps
	// producing frames without ever acking readiness.
	waitCtx, cancel := context.WithTimeout(ctx, bringUpProgressCap)
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
		termination := d.faultTermination
		m.mu.Unlock()
		return &bringUpFailure{kind: bringUpFailureVendor, cause: fmt.Errorf("session-controller: session %s for workspace %q died during bring-up: %s", d.sessionID, d.workspace, reason), queryTermination: termination}
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

	// RUNG 4. The retry below is the ladder's last rung, and this is the bound
	// on how many times the whole ladder may be climbed for one session. The
	// failure in hand is not yet recorded, so it is counted here: reaching the
	// bound with it means this failure is the last one, and it resolves
	// terminally through rung 3's own publication rather than spawning again.
	if m.bringUpFailuresFor(d.sessionID)+1 >= bringUpGiveUpAfter {
		m.logf("session-controller: bring-up GIVING UP ws=%q session=%s generation=%s failure_kind=%s consecutive_failures=%d bound=%d resume=%q resume_decision=preserve retry=false cause=%v — every further respawn would cost another bring-up timeout, so the session parks until it wires or is hibernated",
			workspace, d.sessionID, d.generationID, kind, m.bringUpFailuresFor(d.sessionID)+1, bringUpGiveUpAfter, d.resumedVendorSessionID, cause)
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
	// costs nothing when the connection was never established. That same
	// exception is the one that can leave a terminal result held, which is why
	// the shared prologue's release belongs on this path too.
	m.drainAndCancelSessionController(workspace, d, StopCauseBringUpFailed())
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
	failures, givenUp, cooldown := m.noteBringUpFailure(d.sessionID)
	m.logf("session-controller: START FAILED ws=%q session=%s generation=%s connectivity=unavailable consecutive_failures=%d bound=%d given_up=%t park_cooldown=%s cause=%v branch=bring_up_failed",
		workspace, d.sessionID, d.generationID, failures, bringUpGiveUpAfter, givenUp, cooldown, cause)
	// THE GIVE-UP RIDES THE EXISTING CARD rather than adding a second one. The
	// card identity is stable per session, so the user sees one failure that has
	// gained an account of why nothing is retrying it, not a pile of them.
	//
	// The card names the WAY OUT, not only the wall. A park that expires is only
	// useful to a user who is told it expires, and the card is the one surface
	// that reaches them.
	carded := cause
	if givenUp {
		carded = fmt.Errorf("%w after %d consecutive failures — retrying automatically when it is next opened after %s, or immediately on a hard restart. Last failure: %v",
			ErrBringUpGaveUp, failures, cooldown, cause)
	}
	if d.consumer != nil {
		d.consumer.pushFailure(d.consumer.startFailedUUID(), errclass.StartFailed(carded.Error()))
	}
	m.noteConnectivity(workspace, d.sessionID, d.generationID, ssm.SessionConnectivityUnavailable, "bring_up_failed")
	// THE BUDGET IS NOW SPENT BY SOMEBODY (bringupretry.go). Until this line a
	// failure with attempts remaining had no consumer: every caller of the
	// ladder is a user action or the one-shot boot walk, so a workspace nobody
	// opened sat unwired with `given_up=false` for as long as the daemon lived.
	if m.armBringUpRetry(workspace, d.sessionID, failures, givenUp, cooldown) {
		m.publishBringUpGaveUpCard(workspace, d.sessionID, failures, cooldown, cause)
	}
}
