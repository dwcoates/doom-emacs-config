package sessiondrv

import "claude-repld/internal/ssm"

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
//   - DRIVER EXIT — `client.Run` returned, so this driver is done. It is the
//     catch-all: a terminal protocol error, a cancelled root context, and the
//     cancel a hibernation issues all land here.
//   - HIBERNATION — the shim is SIGTERMed. Closed at the teardown itself rather
//     than left to the exit that follows, because the two are not the same
//     instant and the honest answer is available at the earlier one.
//   - SHIM STOP for one named session, which is hibernation's session-scoped
//     twin and closes for the same reason.
//   - A ROTATION BOUNCE — the vendor retired one transcript identity and the
//     shim re-handshakes. The window between the announcement and the new
//     ShimReady is a real gap in the wiring, so it is reported as one; the
//     re-handshake's own `onConnected` re-opens the axis.
//
// `starting` is written at BRING-UP, and it means strictly that a bring-up is in
// flight — not that a session is wanted. That is what keeps INIT's spinner
// honest: a workspace nobody is bringing up is `dormant`, which spins nothing.

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
