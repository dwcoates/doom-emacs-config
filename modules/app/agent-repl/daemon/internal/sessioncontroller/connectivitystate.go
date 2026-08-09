package sessioncontroller

import (
	"errors"
	"fmt"

	"claude-repld/internal/ssm"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// connectivitystate.go — this package's production of session connectivity.
//
// WHY HERE. A workspace's color is CONNECTION TRUTH: blue means there is no
// live backend session for it, and every non-blue color is a GUARANTEE that the
// session substrate is fully wired — shim live, handshake complete, store link
// settled. Exactly one component in the daemon can witness that, and it is this
// one: it owns the bring-up, holds the shimclient, and is told when the gate
// closes and when the session controller dies.
//
// THE OPENING EDGE IS THE GATE'S OWN VERDICT, not a second opinion about it.
// `onConnected` fires from the shim's ShimReady — the last frame of the bring-up
// gate, and the very thing `AwaitReady` resolves on — so "wired" here means
// precisely what "driveable" means everywhere else in this package. Deriving it
// from anything weaker (a live connection, a spawned process) would put the
// guarantee back where it was before the gate existed: a claim about frames
// being writable rather than about a session being usable.
//
// THE CLOSING EDGES ARE EVERY WAY THAT VERDICT STOPS HOLDING, and they are
// listed here in one place so a new one cannot be added without joining them:
//
//   - SESSION CONTROLLER EXIT — `client.Run` returned a NON-NIL error, so this session controller died
//     on a terminal protocol error. Reported `severed`.
//
//     It used to be the catch-all for every way Run could end, and that was the
//     single most dangerous line in this package once the axis's closed half
//     split in two: a hibernation's own cancel ends Run milliseconds after the
//     hibernation writes `hibernated`, so an unconditional severance at the exit
//     repainted every single hibernation blue immediately after it went teal.
//     `client.Run` loops forever across benign disconnects and returns non-nil
//     only for a terminal protocol error, so the error itself is the
//     discriminator, and a CLEAN exit now writes nothing at all. See the
//     enumeration of controller-ctx cancels at the tail in sessioncontroller.go: every one of
//     them has already recorded a truer answer than the tail could.
//   - HIBERNATION — the shim is SIGTERMed. Closed at the teardown itself rather
//     than left to the exit that follows, because the two are not the same
//     instant and the honest answer is available at the earlier one.
//   - SHIM STOP for one named session, which is hibernation's session-scoped
//     twin and closes for the same reason.
//   - A ROTATION BOUNCE — the vendor retired one transcript identity and the
//     shim re-handshakes. The window between the announcement and the new
//     ShimReady is a real gap in the wiring, so it is reported as one; the
//     re-handshake's own `onConnected` re-opens the axis.
//   - A LINK LOSS WITHOUT A SESSION CONTROLLER EXIT — the shim connection dropped and the
//     shimclient's reconnect loop took over. This was the one closing edge the
//     list was missing, and its absence was not cosmetic: the session controller lives on
//     across a reconnect, so nothing else in this package ever hears about the
//     drop, and the workspace kept claiming to be fully wired for the whole
//     reconnect. It is reported as `starting` rather than `dormant` because a
//     bring-up genuinely IS in flight — the reconnect loop re-runs the entire
//     gate — and the re-handshake's `onConnected` closes it again.
//
// EVERY CLOSING EDGE MUST SAY WHICH CLOSED HALF IT MEANS, because the axis's
// closed half is two tokens rather than one. `severed` claims something broke;
// `hibernated` claims nothing did. One token used to serve both and therefore
// served neither — the idle sweeper reclaiming ~500MB from an untouched
// workspace painted a tab exactly like a dead shim, so blue stopped meaning
// anything. Of the edges above, hibernation and the session-scoped stop are
// benign; controller exit and link loss are not.
//
// `starting` is written at BRING-UP, and it means strictly that a bring-up is in
// flight — not that a session is wanted. That is what keeps INIT's spinner
// honest: a workspace nobody is bringing up is on the axis's closed half, which
// spins nothing.

// noteConnectivity appends one identity-complete connectivity lifecycle edge.
// A rejected edge is never hidden: the caller continues its process teardown
// or bring-up, but the canonical log carries the exact controller identity,
// requested transition, cause, and SSM verdict.
//
// Must be called with m.mu RELEASED: it takes the SSM's lock.
func (m *Manager) noteConnectivity(workspace, sessionID, generationID string, state ssm.SessionConnectivity, causeKind string) error {
	if workspace == "" || sessionID == "" || generationID == "" {
		err := fmt.Errorf("session-controller: incomplete connectivity identity")
		m.logf("session-controller: connectivity edge REJECTED ws=%q session=%q generation=%q next=%q cause=%q branch=incomplete_identity",
			workspace, sessionID, generationID, state, causeKind)
		return err
	}
	if err := m.cfg.SSM.ApplySessionConnectivity(workspace, sessionID, generationID, state, causeKind); err != nil {
		m.logf("session-controller: connectivity edge FAILED ws=%q session=%q generation=%q next=%q cause=%q branch=ssm_rejected error=%v",
			workspace, sessionID, generationID, state, causeKind, err)
		return fmt.Errorf("session-controller: apply connectivity edge for workspace %q session %s generation %s: %w", workspace, sessionID, generationID, err)
	}
	m.logf("session-controller: connectivity edge APPLIED ws=%q session=%q generation=%q next=%q cause=%q branch=accepted",
		workspace, sessionID, generationID, state, causeKind)
	return nil
}

// onLinkLost reports the FIFTH closing edge: the shim link dropped while this
// controller lives on, so the shimclient's reconnect loop is re-running the whole
// bring-up gate.
//
// It fires ONLY for a genuine loss — the shimclient withholds it for a
// teardown-initiated close, where the session controller exit is the honest edge — so this
// never races the row a hibernation or a manager close writes.
//
// The workspace is only moved when this session controller still OWNS it. A superseded
// session controller's link dying says nothing about the replacement now driving the
// workspace, and re-spinning that replacement would be a lie about a live
// session — the same guard the session-controller-exit tail keeps.
func (m *Manager) onLinkLostForGeneration(workspace, sessionID, generationID string, cause error) {
	m.mu.Lock()
	d, ok := m.byWS[workspace]
	current := ok && d.sessionID == sessionID && d.generationID == generationID
	m.mu.Unlock()
	if !current {
		m.logf("session-controller: stale shim link-loss ignored ws=%q session=%q generation=%q cause=%v branch=retired_controller",
			workspace, sessionID, generationID, cause)
		return
	}
	m.logf("session-controller: shim link LOST ws=%q session=%q generation=%q cause=%v branch=reconnect",
		workspace, sessionID, generationID, cause)
	m.noteConnectivity(workspace, sessionID, generationID, ssm.SessionConnectivityConnecting, "link_lost")
}

// currentControllerGeneration is a focused test seam for legacy white-box
// tests that drive callbacks directly. Production callbacks always capture the
// generation at construction and never resolve it ambiently.
func (m *Manager) currentControllerGeneration(workspace, sessionID string) string {
	m.mu.Lock()
	defer m.mu.Unlock()
	d, ok := m.byWS[workspace]
	if !ok || d.sessionID != sessionID {
		return ""
	}
	return d.generationID
}

func (m *Manager) onLinkLost(workspace, sessionID string, cause error) {
	m.onLinkLostForGeneration(workspace, sessionID, m.currentControllerGeneration(workspace, sessionID), cause)
}

// ErrNotSettled reports that a workspace was asked to hibernate while it was
// still WORKING: a turn in flight, a context cut running, or a turn that ended
// on the vendor and has not been superseded.
//
// It is a sentinel rather than an ad-hoc error because refusing a hibernation is
// a routine, expected answer for a caller that sweeps broadly — the idle sweeper
// walks every registry record — and a caller has to be able to tell "not now"
// apart from "the teardown failed" without matching on message text.
//
// It lives here rather than in errclass because nothing classifies it into a
// user-facing failure card: it names a refusal of a daemon-internal operation,
// not a break in the route to the agent, and the workspace it refuses is by
// definition still perfectly usable.
var ErrNotSettled = errors.New("session-controller: the workspace has not settled; refusing to hibernate it")

// redStates are the resolved states that mean A TURN IS IN FLIGHT: `thinking`
// plus the two context cuts, which differ from it only in WHAT the agent is busy
// with. All three make one claim, so all three are treated as one fact here.
var redStates = map[frontendv1.RenderState]bool{
	frontendv1.RenderState_RENDER_STATE_THINKING:   true,
	frontendv1.RenderState_RENDER_STATE_CLEARING:   true,
	frontendv1.RenderState_RENDER_STATE_COMPACTING: true,
}

// unsettledStates are the resolved states that mean the workspace is not done
// working, keyed by RenderState so the check reads off the SSM's own verdict
// rather than re-deriving anything.
//
// THE RED BAND, because each of those is a turn in flight, and hibernating one
// SIGTERMs a shim mid-turn and throws the work away.
//
// AND PURPLE, which is the less obvious half. `vendor_blocked` is a turn OUTCOME
// rather than a live turn, so nothing is running — but it is a report the user
// has not seen through yet, and it is the ONE state whose whole purpose is to
// tell them something needs their attention. Reaping the session under it
// replaces that report with a teal tab claiming everything is fine and asleep,
// which is the exact opposite of what the purple was saying.
var unsettledStates = func() map[frontendv1.RenderState]bool {
	m := map[frontendv1.RenderState]bool{
		frontendv1.RenderState_RENDER_STATE_VENDOR_BLOCKED: true,
		// DEPRECATED upstream and no longer resolved, but still refused: an older
		// daemon's log can resolve it, and it always meant what vendor_blocked
		// means.
		frontendv1.RenderState_RENDER_STATE_STOP_FAILED: true,
	}
	for state := range redStates {
		m[state] = true
	}
	return m
}()

// staleTurnReasonOrphaned names the reconciliation in the closing row's cause.
//
// It is distinct from every teardown token in the stop vocabulary because it
// describes the OPPOSITE situation: no stop is happening and no shim is being
// killed. A turn claim was left standing by a shim that died mid-turn, and the
// daemon has proved — no controller, no process holding the workspace — that
// nothing can ever report its end.
const staleTurnReasonOrphaned = "orphaned_turn_no_live_shim"

// acquireSettledHibernationLease returns ErrNotSettled when the workspace is
// working and otherwise excludes every new prompt/turn start until its release
// function runs. The SSM owns the snapshot and exclusion under one lock, so no
// turn can begin between the settled verdict and StopShim.
//
// A refusal caused by a turn claim gets ONE reconciliation attempt before it
// stands (reconcileOrphanedTurn), because a claim whose reporter is gone would
// otherwise refuse every hibernation for that workspace forever.
func (m *Manager) acquireSettledHibernationLease(workspace string) (func(), error) {
	return m.acquireSettledHibernationLeaseAfter(workspace, false)
}

// reconcileOrphanedTurn closes a workspace's standing turn claim when the party
// that could report its end is PROVABLY gone, and reports whether it did.
//
// # The wedge
//
// `state=hibernated` with `turn_active=true` is self-contradictory: hibernation
// SIGTERMs the shim, so a hibernated session cannot have a live turn. The flag
// is what a shim killed mid-turn leaves behind. Only a successful shim
// handshake clears it (the `shim_handshake_no_turns` close), so when the
// handshake is what keeps failing the workspace can never settle, can never
// hibernate, and the pair persists across every restart after it.
//
// # The proof, and why nothing weaker will do
//
// A turn is never GUESSED dead. Two facts must both hold, and either being
// unavailable is a refusal:
//
//   - the daemon drives NO controller for the workspace; and
//   - NO process holds the workspace's kernel lock, which covers a live shim
//     that has not dialled in as well as one that has.
//
// The second is the load-bearing half. Connection tracking alone answers "not
// connected" for a surviving shim still inside its reconnect backoff, and
// closing a turn under a shim that is genuinely mid-turn would paint a running
// turn idle.
//
// The close itself goes through the SAME mechanism every teardown uses, so
// there is exactly one writer of a synthesized closing row; only the reason
// distinguishes this cause from a teardown's.
//
// # Why the close is workspace-scoped rather than session-scoped
//
// A teardown's close retires the claims of the ONE session whose shim it
// stopped. This reconciliation's proof is stronger and differently shaped —
// nothing is alive to end ANY turn here — and using the narrower close under it
// is what made the observed churn: when the standing claim belonged to a
// claimant other than the resolved state's session, the close retired nothing,
// the workspace-wide liveness fold still held a turn open, and the workspace
// stayed `thinking` with no shim while this ran again every sweep. So it calls
// CloseOrphanedTurn, which retires the whole workspace's ledger in the same
// transaction that reconciles the axis.
func (m *Manager) reconcileOrphanedTurn(workspace string, st *frontendv1.WorkspaceState) bool {
	m.mu.Lock()
	d, live := m.byWS[workspace]
	m.mu.Unlock()
	if live {
		// A LIVE CONTROLLER IS NOT AN UNCONDITIONAL SHIELD, and treating it as
		// one is what let the deadlock stand. See reconcileUnsubstantiatedTurn:
		// the decline holds unless the SHIM ITSELF has contradicted the claim,
		// and it is the shim's own evidence that narrows it, never a guess about
		// what a live controller might be doing.
		if m.reconcileUnsubstantiatedTurn(workspace, d, st) {
			return true
		}
		m.logf("session-controller: orphaned-turn reconciliation DECLINED ws=%q session=%s — a live controller drives this workspace and its shim has not contradicted the claim, so its turn claim is not provably stale",
			workspace, d.sessionID)
		return false
	}
	held, err := m.workspaceLockHeld(workspace)
	if err != nil {
		m.logf("session-controller: orphaned-turn reconciliation REFUSED ws=%q — cannot determine whether a shim is alive for it (%v). A turn is never guessed dead, so the claim stands.",
			workspace, err)
		return false
	}
	if held {
		m.logf("session-controller: orphaned-turn reconciliation DECLINED ws=%q — a live shim holds this workspace's lock, so the turn may be genuinely running even though nothing has dialled in",
			workspace)
		return false
	}
	sessionID := st.GetSessionId()
	closed, err := m.cfg.SSM.CloseOrphanedTurn(workspace, sessionID, staleTurnReasonOrphaned)
	if err != nil {
		m.logf("session-controller: orphaned-turn reconciliation FAILED ws=%q session=%q reason=%s: %v — the workspace stays latched in a turn it cannot leave",
			workspace, sessionID, staleTurnReasonOrphaned, err)
		return false
	}
	if !closed {
		m.logf("session-controller: orphaned-turn reconciliation WROTE NOTHING ws=%q session=%q reason=%s — the workspace held no open claim and its axis carried no live turn, so there was nothing stale to reconcile",
			workspace, sessionID, staleTurnReasonOrphaned)
		return false
	}
	m.logf("session-controller: orphaned turn CLOSED ws=%q session=%q reason=%s state=%s — no controller and no process hold this workspace, so the turn's end can never be reported and the claim was reconciled",
		workspace, sessionID, staleTurnReasonOrphaned, st.GetState())
	return true
}

// acquireSettledHibernationLeaseAfter is the lease acquisition itself.
// reconciled records that the orphaned-turn reconciliation has already run for
// this request, which bounds it to one attempt.
func (m *Manager) acquireSettledHibernationLeaseAfter(workspace string, reconciled bool) (func(), error) {
	st, found, release, err := m.cfg.SSM.AcquireHibernationLease(workspace)
	if err != nil {
		if errors.Is(err, ssm.ErrControllerRegistrationInProgress) {
			m.logf("session-controller: REFUSING to hibernate ws=%q — a controller generation owns bring-up admission", workspace)
			return nil, fmt.Errorf("%w: workspace %q has a controller generation entering service", ErrNotSettled, workspace)
		}
		m.logf("session-controller: hibernation lease acquisition FAILED ws=%q outcome=refuse_unknown_state error=%v", workspace, err)
		return nil, fmt.Errorf("session-controller: acquire hibernation lease for workspace %q: %w", workspace, err)
	}
	if !found {
		release()
		m.logf("session-controller: REFUSING to hibernate ws=%q — the SSM has no resolved state, so settledness is unproved", workspace)
		return nil, fmt.Errorf("%w: workspace %q has no resolved state", ErrNotSettled, workspace)
	}
	state := st.GetState()
	if !st.GetTurnActive() && !unsettledStates[state] {
		return release, nil
	}
	release()
	m.logf("session-controller: REFUSING to hibernate ws=%q — it has not settled (state=%s turn_active=%v). Hibernating it would SIGTERM a shim that is still working and paint the workspace asleep over a live turn",
		workspace, state, st.GetTurnActive())
	// ONE CHANCE TO BECOME LEGITIMATE. A turn claim whose reporter is gone
	// refuses every hibernation of this workspace for as long as the daemon
	// lives, so the refusal caused by one is where the claim is tested against
	// the liveness that would justify it. The reconciliation refuses on its own
	// account whenever it cannot PROVE the turn dead, and it runs at most once
	// per request, so the retry below cannot recur.
	if !reconciled && st.GetTurnActive() && m.reconcileOrphanedTurn(workspace, st) {
		return m.acquireSettledHibernationLeaseAfter(workspace, true)
	}
	return nil, fmt.Errorf("%w: workspace %q reads %s (turn_active=%v)", ErrNotSettled, workspace, state, st.GetTurnActive())
}
