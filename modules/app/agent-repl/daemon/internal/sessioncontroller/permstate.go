package sessioncontroller

import "fmt"

// notePermissionState drives the SSM's permission row off the workspace's LIVE
// pending-permission count.
//
// THE COUNT IS NOT A SECOND LEDGER. permRegistry already holds exactly one
// waiter per parked canUseTool round-trip, tagged with its workspace, created
// the moment the shim asks and removed the moment the request is released — by
// a grant, a deny, or a teardown abandonment alike. That set IS the answer to
// "is the agent waiting on this user", so the producer reads it rather than
// counting edges of its own, and no resolution path can be forgotten because
// none of them is enumerated here.
//
// It is called on BOTH edges of every permission's life, from the same
// notification the progress footer's pending badge rides (permHandler's
// onPermsChanged): once after the waiter parks, once after it is released. The
// SSM decides whether either is a real edge — the first pending request opens
// the row and only the count reaching zero closes it, so a second concurrent
// permission moves nothing and its answer moves nothing while the first is
// still parked.
//
// THE COUNT IS READ AFTER THE SET MOVES, not captured as the caller changed it,
// so a permission answered before its own opening edge is applied reports zero
// and opens nothing. That is correct rather than merely tolerable: the row
// exists to say a human is being waited on, and nobody was. The end state is
// identical either way — the SSM's close finds nothing open and says so.
//
// Must be called with m.mu RELEASED: it takes the permission registry's lock
// and then the SSM's.
func (m *Manager) notePermissionState(workspace string) {
	pending := len(m.reg.idsForWorkspace(workspace))
	// The reason rides the row as its cause detail, so the log names WHY the
	// workspace changed color rather than only that it did.
	reason := fmt.Sprintf("pending=%d", pending)
	if err := m.cfg.SSM.ApplyPermission(workspace, pending > 0, reason); err != nil {
		// Loud, never swallowed: a failed edge here is a workspace whose color
		// stops tracking what the agent is waiting for, and the permission
		// round-trip it belongs to must still complete.
		m.logf("session-controller: applying the permission row to the SSM FAILED ws=%s pending=%d: %v", workspace, pending, err)
	}
}
