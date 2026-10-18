package server

import (
	"context"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/ids"
	"agent-repld/internal/publish"
)

// A SELECTION THAT NAMES A ROW REVEALS IT ON THE SELECTED WORKSPACE'S PAGE
// (SelectWorkspaceRequest.reveal; owner ruling, 2026-10-17: an async merge's
// queue entry opens its bubble in the requester). The reveal is an EVENT -- a
// page that reloads later must not be moved again -- so it rides publish.Events,
// which replays nothing. A page that is not listening when it is asked (its
// page was not loaded, and the selection is what loads it) is handed it FIRST
// when it attaches: the reveal is kept as the workspace's one pending reveal
// until a stream takes it. Both sides decide under webRevealsMu, so a reveal
// can never fall between a stream subscribing and its reading of the pending
// one.

// webReveal is one workspace's reveal events and the reveal no stream has
// taken yet.
type webReveal struct {
	events  publish.Events[*frontendv1.FeedId]
	pending *frontendv1.FeedId
}

// webRevealOf answers ws's reveal state, minting it on first use. The caller
// holds webRevealsMu.
func (s *server) webRevealOf(ws ids.WorkspaceID) *webReveal {
	r, ok := s.webReveals[ws]
	if !ok {
		r = &webReveal{}
		s.webReveals[ws] = r
	}
	return r
}

// revealRow asks ws's page to reveal ROW: emitted to every stream listening,
// or kept for the next one to attach when none is.
func (s *server) revealRow(log dlog.Logger, ws ids.WorkspaceID, row *frontendv1.FeedId) {
	const op = "daemon.server.reveal_row"
	s.webRevealsMu.Lock()
	r := s.webRevealOf(ws)
	listening := r.events.Subscribers() > 0
	if listening {
		r.pending = nil
		r.events.Emit(row)
	} else {
		r.pending = row
	}
	s.webRevealsMu.Unlock()
	if listening {
		log.Info(op, "asked the workspace's page to reveal a row", dlog.Context{"row": row.GetValue()})
		return
	}
	log.Info(op, "the workspace's page is not listening; the row is revealed when it attaches", dlog.Context{"row": row.GetValue()})
}

// subscribeReveals subscribes a web stream to ws's reveals for as long as ctx
// stands, and takes the pending one, if any, which the stream sends first.
func (s *server) subscribeReveals(ctx context.Context, ws ids.WorkspaceID) (<-chan *frontendv1.FeedId, *frontendv1.FeedId) {
	s.webRevealsMu.Lock()
	defer s.webRevealsMu.Unlock()
	r := s.webRevealOf(ws)
	reveals := r.events.Subscribe(ctx)
	pending := r.pending
	r.pending = nil
	return reveals, pending
}

// revealPush is the web stream's push asking the page to reveal ROW.
func revealPush(row *frontendv1.FeedId) *agentreplv1.WatchWebWorkspaceResponse {
	return &agentreplv1.WatchWebWorkspaceResponse{
		Push: &agentreplv1.WatchWebWorkspaceResponse_RevealRow{RevealRow: &agentreplv1.WebWorkspaceRevealRow{Row: row}},
	}
}
