package frontend

// restartpending.go — delivery of the daemon's INTENTIONAL-RESTART
// announcement, the last frame a client sees before the listeners close.
//
// WHY THIS IS ITS OWN DELIVERY PATH rather than another Push helper over
// Broadcast: every other push reports one undifferentiated count, and for this
// frame the count is the whole diagnostic. The announcement exists so a
// deliberate bounce stops looking like a crash, and it fails PER CARRIER —
// the webapp can be told while Emacs is not, or the reverse. A single total
// cannot say which happened, so the shutdown log could only report a vague
// partial delivery for the one event whose entire purpose is explaining an
// outage that is about to start.
//
// The frame is DAEMON-GLOBAL and unscoped: it names no workspace and no
// session, so it is never held by the publication latch and never filtered by
// a client's scope. That matters at this call site more than anywhere else —
// a parked announcement would be flushed by a release that will never run,
// because the process is exiting.

import (
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
)

// RestartPendingFrame wraps the intentional-restart announcement.
//
// It is NOT host-only. Both frontends alarm on an unexplained disconnect, so
// both are entitled to hear that this one is deliberate.
func RestartPendingFrame(v *frontendv1.RestartPendingView) *frontendv1.FrontendFrame {
	return &frontendv1.FrontendFrame{Frame: &frontendv1.FrontendFrame_RestartPending{RestartPending: v}}
}

// PushRestartPendingToGUI delivers the announcement to every GUI connection
// (bootstrap, stream and observer alike) and reports how many were told.
//
// Every GUI kind is included deliberately: each one renders the severed
// banner on a dead socket, so each one needs the fact that would suppress it.
func (s *Server) PushRestartPendingToGUI(v *frontendv1.RestartPendingView) int {
	return s.broadcastGatedTo(RestartPendingFrame(v), func(k ClientKind) bool { return !k.isHost() })
}

// PushRestartPendingToHost delivers the announcement to the Emacs UDS host
// connection and reports how many host clients were told.
//
// A zero is MEANINGFUL, not a shrug: it means Emacs was not connected at
// teardown and will render this deliberate bounce as a degraded link. The
// caller turns that into a loud line rather than discarding it.
func (s *Server) PushRestartPendingToHost(v *frontendv1.RestartPendingView) int {
	return s.broadcastGatedTo(RestartPendingFrame(v), ClientKind.isHost)
}
