package ssm

import (
	corev1 "agentrepl/proto/agentshim/core/v1"
)

// applyTest routes ONE event exactly the way the daemon's consumer does: a turn
// boundary through ApplyTurnBoundary, everything else through Apply.
//
// It exists because the tests predate the split and were written against a
// single Apply. It is NOT a bypass — a turn boundary still travels the one
// boundary path, and the workspace/claimant it is filed under come from the same
// resolver the daemon uses. An event whose session no resolver binds is passed
// through so the caller sees the resolver's own refusal rather than a validation
// error about a workspace the test never supplied.
func applyTest(m *Manager, ev *corev1.Event) error {
	switch ev.GetPayload().(type) {
	case *corev1.Event_TurnStarted, *corev1.Event_TurnEnded:
	default:
		return m.Apply(ev)
	}
	workspace, claimant := "unbound", "unbound"
	if m.resolver != nil {
		if binding, ok := m.resolver.Session(ev.GetSessionId()); ok {
			workspace, claimant = binding.Workspace, binding.SessionID
		}
	}
	_, err := m.ApplyTurnBoundary(workspace, claimant, "", ev)
	return err
}

// ResolveTurnLifecycle is the ledger-shaped view of ApplyTurnBoundary that the
// existing boundary tests are written against.
//
// IT IS A TEST FIXTURE, NOT A SECOND DOOR. It exists only in the test binary and
// it does not bypass anything: every call runs the ONE boundary path, so a test
// asserting on the ledger is asserting on the same transaction that paints the
// color. A production caller has exactly one way in, which is the point of
// Apply's refusal.
func (m *Manager) ResolveTurnLifecycle(workspace, claimantSessionID, liveQueryInstanceID string, ev *corev1.Event) (before, after []string, replayed bool, err error) {
	b, err := m.ApplyTurnBoundary(workspace, claimantSessionID, liveQueryInstanceID, ev)
	return b.Before, b.After, b.Replayed, err
}
