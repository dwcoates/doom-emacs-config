package workspace

import (
	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/ids"
	"claude-repld/internal/resolve/feed"
	"claude-repld/internal/resolve/topbar"
)

// THE SERVED-CARD RECORD.
//
// Every answer verb echoes an ask back at the shim, and the client sends only
// the ask's IDENTITY. What the daemon actually served — which agent is blocked,
// which standing token the vendor offered, which batch the user saw, which
// modes the picker offered — lives in the resolvers that drew those cards. The
// Cards surface is that record read back, assembled from the three producers
// that hold it rather than kept a second time here.

// cards answers Deps.Cards from the resolvers and the fleet that served the
// cards. It holds NO state of its own: a second copy of what was served is a
// second opinion about it.
type cards struct {
	feed   feed.Resolver
	topbar topbar.Resolver
	fleet  *Fleet
}

// NewCards assembles the served-card record from its three producers: the feed
// resolver drew the permission and question cards, the fleet raised the cold
// gate, and the topbar served the permission-mode picker.
func NewCards(feedResolver feed.Resolver, topbarResolver topbar.Resolver, fleet *Fleet) Cards {
	return &cards{feed: feedResolver, topbar: topbarResolver, fleet: fleet}
}

// Permission answers what was served for one permission ask.
func (c *cards) Permission(ws ids.WorkspaceID, id *conversationv1.AgentPermissionId) (ServedPermission, bool) {
	agent, standing, ok := c.feed.ServedPermission(ws, id.GetValue())
	if !ok {
		return ServedPermission{}, false
	}
	return ServedPermission{Agent: agent, StandingFor: standing}, true
}

// Question answers what was served for one question ask.
func (c *cards) Question(ws ids.WorkspaceID, id *conversationv1.AgentQuestionId) (ServedQuestion, bool) {
	agent, batch, ok := c.feed.ServedQuestion(ws, id.GetValue())
	if !ok {
		return ServedQuestion{}, false
	}
	return ServedQuestion{Agent: agent, Batch: batch}, true
}

// ColdGate answers the menu a standing cold gate served. The FLEET raised it,
// so the fleet is what remembers it.
func (c *cards) ColdGate(ws ids.WorkspaceID) (ServedColdGate, bool) {
	return c.fleet.ColdGate(ws)
}

// TakeColdGate spends an answered gate where it lives, in the fleet.
func (c *cards) TakeColdGate(ws ids.WorkspaceID, vendorSessionID string) bool {
	return c.fleet.TakeColdGate(ws, vendorSessionID)
}

// EndColdGate retires a remediated gate where it lives, in the fleet.
func (c *cards) EndColdGate(ws ids.WorkspaceID, vendorSessionID string) {
	c.fleet.EndColdGate(ws, vendorSessionID)
}

// ReraiseColdGate stands a failed answer's gate again, in the fleet.
func (c *cards) ReraiseColdGate(ws ids.WorkspaceID, vendorSessionID string) bool {
	return c.fleet.ReraiseColdGate(ws, vendorSessionID)
}

// Models answers exactly the model catalog the topbar's selector served.
func (c *cards) Models(ws ids.WorkspaceID) ([]string, bool) {
	return c.topbar.ModelCatalog(ws)
}

// PermissionModes answers exactly the switchable mode set the topbar served.
func (c *cards) PermissionModes(ws ids.WorkspaceID) ([]string, bool) {
	return c.topbar.PermissionModes(ws)
}
