package sessioncontroller

import (
	"context"

	protocolv1 "agentrepl/proto/protocol/v1"

	"claude-repld/internal/shimclient"
	"claude-repld/internal/storehistory"
)

// THE LIVE ROUTE IS NOW BOUNDED TOO, and it gets there THROUGH THE SHIM.
//
// # What this changes, and what it deliberately does not
//
// A workspace with a live session controller is the COMMON case — every
// workspace anyone is actually using — and it was the one still served by the
// windowed backwards walk (conversationpage.go): a forward read from a guessed
// lower bound, widened until it happened to cover ten messages. Every cost this
// wave removed applied to the unwired case only.
//
// The walk is UNCHANGED and still reachable: the older ConversationPage surface
// (servePage) still takes it, and nothing here deletes it. What changed is
// which route the POSITIONLESS history surface takes for a live workspace.
//
// # WHY THROUGH THE SHIM, and not by dialling the store
//
// storehistory.Reader can dial the store socket directly, and for an unwired
// workspace that is the only route there is. Using it while a shim is UP would
// be the side door repull.go forbids: history served around the session's own
// transport masks a shim outage instead of surfacing it. So the daemon asks the
// shim, which asks the store and re-stamps the page with the daemon's request
// id — a pass-through that opens no StoredMessage.
//
// # ONE CURATOR, still
//
// Nothing here translates a StoredMessage into a feed message. The page is
// handed to pageFromMessagePage, which unpacks the records it already carries
// and drives them through consumer.pushConversation — the same chokepoint the
// unwired route, the durable replay and the shim re-pull all funnel through. A
// paged message is therefore byte-identical in shape to a pushed one, because
// the same code made both.

// pageFromControllerPage serves one history page for a workspace with a LIVE
// session controller, reading the bounded page THROUGH THE SHIM.
func (m *Manager) pageFromControllerPage(ctx context.Context, d *sessionController, generationID string, resolve pageBoundResolver, first bool) (pageOutcome, error) {
	fetch := func(ctx context.Context, anchor storehistory.PageAnchor) (*protocolv1.MessagePage, error) {
		// The anchor crosses the package boundary unchanged: a head arm that
		// names nothing, or a before_seq the STORE minted and this daemon kept.
		// Nothing is computed here, which is the point.
		return d.client.MessagePage(ctx, shimclient.MessagePageAnchor{Head: anchor.Head, BeforeSeq: anchor.BeforeSeq})
	}
	return m.pageFromMessagePage(ctx, d.workspace, d.sessionID, generationID, "shim-page", m.lastSeenSeq(d), fetch, resolve, first)
}
