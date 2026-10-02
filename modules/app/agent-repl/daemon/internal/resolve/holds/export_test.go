package holds

import (
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/ids"
)

// HeldBadges exposes the badge composer to the external test package, so every
// arm — including ones no durable record can reach — is tested directly.
var HeldBadges = heldBadges

// TruncateLabel exposes the command truncation for the same reason.
var TruncateLabel = truncateLabel

// RenderNow renders ws's tray from the resolver's current state, under its
// lock, so a test can compare what is published with what the state says.
func RenderNow(r Resolver, ws ids.WorkspaceID) *frontendv1.DaemonHoldTray {
	res := r.(*resolver)
	res.mu.Lock()
	defer res.mu.Unlock()
	s := res.stateLocked(ws)
	return res.render(s, res.logOf(ws, s))
}
