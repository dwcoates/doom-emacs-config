package sessiondrv

import (
	"errors"
	"fmt"

	"claude-repld/internal/ssm"

	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// wiredstate.go — this package's production of the SSM's WIRED axis.
//
// WHY HERE. A workspace's color is CONNECTION TRUTH: blue means there is no
// live backend session for it, and every non-blue color is a GUARANTEE that the
// session substrate is fully wired — shim live, handshake complete, store link
// settled. Exactly one component in the daemon can witness that, and it is this
// one: it owns the bring-up, holds the shimclient, and is told when the gate
// closes and when the driver dies.
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
//   - DRIVER EXIT — `client.Run` returned a NON-NIL error, so this driver died
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
//     enumeration of driver-ctx cancels at the tail in driver.go: every one of
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
//   - A LINK LOSS WITHOUT A DRIVER EXIT — the shim connection dropped and the
//     shimclient's reconnect loop took over. This was the one closing edge the
//     list was missing, and its absence was not cosmetic: the driver lives on
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
// benign; driver exit and link loss are not.
//
// `starting` is written at BRING-UP, and it means strictly that a bring-up is in
// flight — not that a session is wanted. That is what keeps INIT's spinner
// honest: a workspace nobody is bringing up is on the axis's closed half, which
// spins nothing.

// noteWiring moves the workspace's wired axis, loud-logging a failure rather
// than swallowing it.
//
// A failed edge here is a workspace whose color stops tracking whether it has a
// session at all, which is the one thing the vocabulary promises — but the
// bring-up or teardown that called it must still complete, so the failure is
// surfaced and the caller carries on. Same shape as notePermissionState.
//
// Must be called with m.mu RELEASED: it takes the SSM's lock.
func (m *Manager) noteWiring(workspace string, wiring ssm.Wiring, reason string) {
	if workspace == "" {
		return
	}
	if err := m.cfg.SSM.ApplyWired(workspace, wiring, reason); err != nil {
		m.logf("sessiondrv: applying the wired axis to the SSM FAILED ws=%q wiring=%s reason=%s: %v",
			workspace, wiring, reason, err)
	}
}

// onLinkLost reports the FIFTH closing edge: the shim link dropped while this
// driver lives on, so the shimclient's reconnect loop is re-running the whole
// bring-up gate.
//
// It fires ONLY for a genuine loss — the shimclient withholds it for a
// teardown-initiated close, where the driver exit is the honest edge — so this
// never races the row a hibernation or a manager close writes.
//
// The workspace is only moved when this driver still OWNS it. A superseded
// driver's link dying says nothing about the replacement now driving the
// workspace, and re-spinning that replacement would be a lie about a live
// session — the same guard the driver-exit tail keeps.
func (m *Manager) onLinkLost(workspace, sessionID string, cause error) {
	m.mu.Lock()
	d, ok := m.byWS[workspace]
	current := ok && d.sessionID == sessionID
	m.mu.Unlock()
	if !current {
		m.logf("sessiondrv: shim link lost on a superseded session=%s ws=%q; leaving the wired axis to the live driver: %v",
			sessionID, workspace, cause)
		return
	}
	m.logf("sessiondrv: shim link LOST session=%s ws=%q — reconnecting, workspace is starting again: %v",
		sessionID, workspace, cause)
	m.noteWiring(workspace, ssm.WiringStarting, "link_lost")
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
var ErrNotSettled = errors.New("sessiondrv: the workspace has not settled; refusing to hibernate it")

// unsettledStates are the resolved states that mean the workspace is not done
// working, keyed by RenderState so the check reads off the SSM's own verdict
// rather than re-deriving anything.
//
// THE RED BAND, because each of the three is a turn in flight: `thinking` plus
// the two context cuts, which differ from it only in WHAT the agent is busy
// with. Hibernating any of them SIGTERMs a shim mid-turn and throws the work
// away.
//
// AND PURPLE, which is the less obvious half. `vendor_blocked` is a turn OUTCOME
// rather than a live turn, so nothing is running — but it is a report the user
// has not seen through yet, and it is the ONE state whose whole purpose is to
// tell them something needs their attention. Reaping the session under it
// replaces that report with a teal tab claiming everything is fine and asleep,
// which is the exact opposite of what the purple was saying.
var unsettledStates = map[frontendv1.RenderState]bool{
	frontendv1.RenderState_RENDER_STATE_THINKING:       true,
	frontendv1.RenderState_RENDER_STATE_CLEARING:       true,
	frontendv1.RenderState_RENDER_STATE_COMPACTING:     true,
	frontendv1.RenderState_RENDER_STATE_VENDOR_BLOCKED: true,
	// DEPRECATED upstream and no longer resolved, but still refused: an older
	// daemon's log can resolve it, and it always meant what vendor_blocked means.
	frontendv1.RenderState_RENDER_STATE_STOP_FAILED: true,
}

// refuseUnsettledHibernation returns ErrNotSettled when the workspace's resolved
// state says it is still working, loud-logging the refusal.
//
// A READ FAILURE ALLOWS THE HIBERNATION, and this is the one place in the
// hibernation path where an unknown answers YES rather than NO. It is the
// opposite of the idle sweeper's elapsed-quiet gate, deliberately, because the
// two guard different things: the sweeper decides whether to hibernate a
// workspace nobody asked about, so absent evidence must not license a teardown,
// while this only VETOES a teardown somebody already decided on. Turning a state
// read failure into a refusal here would make an SSM outage silently disable
// hibernation across the whole daemon, and memory exhaustion is a worse failure
// than one shim reaped a moment early. The failure is surfaced loudly either
// way, never swallowed.
//
// A workspace with NO resolved state is likewise allowed through: it has no turn
// to interrupt, and hibernate's own no-live-driver error is the honest refusal
// for a workspace nothing is driving.
func (m *Manager) refuseUnsettledHibernation(workspace string) error {
	st, found, err := m.cfg.SSM.Current(workspace)
	if err != nil {
		m.logf("sessiondrv: hibernation settled-check FAILED ws=%q: %v — ALLOWING the teardown, because a state read outage must not disable hibernation daemon-wide",
			workspace, err)
		return nil
	}
	if !found {
		return nil
	}
	state := st.GetState()
	if !st.GetTurnActive() && !unsettledStates[state] {
		return nil
	}
	m.logf("sessiondrv: REFUSING to hibernate ws=%q — it has not settled (state=%s turn_active=%v). Hibernating it would SIGTERM a shim that is still working and paint the workspace asleep over a live turn",
		workspace, state, st.GetTurnActive())
	return fmt.Errorf("%w: workspace %q reads %s (turn_active=%v)", ErrNotSettled, workspace, state, st.GetTurnActive())
}
