package server

import (
	"context"
	"errors"
	"fmt"
	"time"

	"connectrpc.com/connect"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/feedid"
	"agent-repld/internal/ids"
	"agent-repld/internal/resolve/feed"
)

// The feed's three rpcs. OpenFeed mints the watch token and answers the newest
// page; WatchFeed tails from exactly that token's pin; GetFeedPage answers the
// newest page again, or the page above the oldest row the reader names as
// held. The daemon keeps no walk between requests: the reader's oldest held
// row is the position.

// feedOf resolves an optional FeedId onto the feed it addresses, refusing an
// undecodable value and a feed that belongs to another workspace. An UNSET id
// is the ROOT feed, which is the only feed every workspace always has.
func feedOf(ws ids.WorkspaceID, id *frontendv1.FeedId) (feedid.Feed, *refusal) {
	if id == nil {
		return feedid.Feed{Root: true}, nil
	}
	owner, decoded, err := feedid.DecodeFeed(id)
	if err != nil {
		return feedid.Feed{}, &refusal{
			Arm:    "feed_undecodable",
			Reason: fmt.Sprintf("the feed id %q does not decode: %v", id.GetValue(), err),
		}
	}
	if owner != ws {
		return feedid.Feed{}, &refusal{
			Arm: "feed_not_in_workspace",
			Reason: fmt.Sprintf("the feed id %q belongs to workspace %q, not %q",
				id.GetValue(), owner, ws),
		}
	}
	return decoded, nil
}

// OpenFeed opens ONE feed — the root, or a subagent bubble's sub-feed —
// answering the newest page and MINTING the watch token WatchFeed echoes. The
// token's pin is what keeps a row from falling between the page and the stream.
func (s *server) OpenFeed(
	ctx context.Context,
	req *connect.Request[agentreplv1.OpenFeedRequest],
) (*connect.Response[agentreplv1.OpenFeedResponse], error) {
	const rpc = "OpenFeed"
	if err := validateOpenFeedRequest(req.Msg); err != nil {
		return nil, err
	}
	resp := &agentreplv1.OpenFeedResponse{}
	subject, r, err := s.resolveRef(ctx, rpc, req.Msg.GetWorkspace())
	if err != nil {
		return nil, fail(s.log, rpc, err)
	}
	if r != nil {
		return answer(resp, s.refuse(s.log, rpc, resp, *r))
	}
	target, feedRefusal := feedOf(subject.Record.ID, req.Msg.Feed)
	if feedRefusal != nil {
		return answer(resp, s.refuse(subject.Log, rpc, resp, s.fill(*feedRefusal)))
	}

	started := time.Now()
	page, token, held, err := s.deps.Feed.OpenPage(ctx, subject.Record.ID, target)
	if err != nil {
		if refused, ok := s.asRefusal(err); ok {
			return answer(resp, s.refuse(subject.Log, rpc, resp, refused))
		}
		return nil, fail(subject.Log, rpc, err)
	}
	s.rememberToken(token.GetValue(), subject.Record.ID, target)
	// INFO: an open is a page's first paint waiting on the daemon, once per
	// page load; its duration is half of what the page's own feed.open-answered
	// record measures.
	subject.Log.Info("daemon.server.open_feed", "opened a feed and minted its watch token",
		dlog.Context{"token": token.GetValue(), "rows": len(page.GetSuccess().GetRows()),
			"root": target.Root, "held": len(held.GetItems()),
			"ms": time.Since(started).Milliseconds()})
	resp.Result = &agentreplv1.OpenFeedResponse_Success{
		Success: &agentreplv1.OpenFeedSuccess{Page: page, Watch: token, Held: held},
	}
	return connect.NewResponse(resp), nil
}

// rememberToken records which workspace and feed a minted watch token
// addresses. WatchFeedRequest carries ONLY the token, while the feed resolver's
// Tail takes the workspace and the feed explicitly, so the mint site is the one
// place that still knows both.
func (s *server) rememberToken(token string, ws ids.WorkspaceID, target feedid.Feed) {
	s.mu.Lock()
	defer s.mu.Unlock()
	s.watchTokens[token] = tokenTarget{WS: ws, Feed: target}
}

// tokenTargetFor answers a remembered token's address.
func (s *server) tokenTargetFor(token string) (tokenTarget, bool) {
	s.mu.Lock()
	defer s.mu.Unlock()
	target, ok := s.watchTokens[token]
	return target, ok
}

// WatchFeed tails one feed from the pin its token names: every row upsert, in
// order, ending only when the client cancels.
func (s *server) WatchFeed(
	ctx context.Context,
	req *connect.Request[agentreplv1.WatchFeedRequest],
	out *connect.ServerStream[agentreplv1.WatchFeedResponse],
) error {
	return s.watchFeed(ctx, req.Msg, out)
}

// watchFeed is the body, written to whatever sink carries it: the dedicated
// rpc's own stream, or one page's mux.
func (s *server) watchFeed(
	ctx context.Context,
	msg *agentreplv1.WatchFeedRequest,
	out streamSink[agentreplv1.WatchFeedResponse],
) error {
	const rpc = "WatchFeed"
	if err := validateWatchFeedRequest(msg); err != nil {
		return err
	}
	token := msg.GetWatch()
	target, known := s.tokenTargetFor(token.GetValue())
	if !known {
		return TransportClosed(s.log, rpc, "unknown_token",
			fmt.Sprintf("no feed watch token %q was minted by this daemon", token.GetValue()), true)
	}
	log, err := s.workspaceLog(ctx, rpc, target.WS)
	if err != nil {
		return endStream(s.log, rpc, err)
	}

	streamCtx, cancel := s.streamContext(ctx)
	defer cancel()

	tail, err := s.deps.Feed.Tail(streamCtx, target.WS, target.Feed, token)
	if err != nil {
		switch {
		case errors.Is(err, feed.ErrUnknownToken):
			return TransportClosed(log, rpc, "unknown_token", err.Error(), true)
		case errors.Is(err, feed.ErrTokenExpired):
			return TransportClosed(log, rpc, "token_expired", err.Error(), false)
		}
		return endStream(log, rpc, err)
	}

	pubs := tail.Publications(streamCtx)

	// THE RESPONSE-SELECTION PUSH RIDES THE ROOT FEED'S WATCH ONLY. The
	// FeedSelection frame names rows of THIS feed (the selected/centered
	// final-response rows), and those rows live on the root feed; a sub-feed's
	// watch carries none, so it subscribes to no selection. A nil channel in
	// the select below simply never fires, which is what a non-root feed wants.
	var selections <-chan *selectionState
	if target.Feed.Root {
		selections = s.selectionTopic(target.WS).Subscribe(streamCtx)
	}

	// THE FEED TEXT ZOOM RIDES EVERY FEED'S WATCH. Unlike the selection push,
	// the scale is daemon-global and applies to all feed text, so a sub-feed's
	// watch subscribes too. The topic replays its latest value, so this delivers
	// the current zoom the instant the tail is accepted (Prime seeded it before
	// serving), then every later change.
	scales := s.feedTextScaleTopic.Subscribe(streamCtx)

	// THE IMAGE OVERLAY RIDES THE ROOT FEED'S WATCH ONLY, like the selection:
	// it lies over the workspace's root feed. The topic replays its latest
	// value, so a page opening after the image was opened draws it at once.
	var overlays <-chan *frontendv1.FeedImageOverlay
	if target.Feed.Root {
		overlays = s.imageOverlayTopic(target.WS).Subscribe(streamCtx)
	}

	s.acceptStream(ctx, rpc)
	log.Debug(rpc, "accepted a feed tail", dlog.Context{"token": token.GetValue()})
	for {
		select {
		case <-streamCtx.Done():
			log.Debug(rpc, "the feed tail ended on cancellation", nil)
			return nil
		case pub, ok := <-pubs:
			if !ok {
				log.Debug(rpc, "the feed tail's subscription closed", nil)
				return nil
			}
			frame, sendable := feedFrame(pub)
			if !sendable {
				log.Error(rpc, "the feed resolver raised a publication carrying neither a row nor a held change; it was not sent", nil)
				continue
			}
			if err := out.Send(frame); err != nil {
				log.Debug(rpc, "the feed tail's client went away", dlog.Context{"cause": err.Error()})
				return nil
			}
		case sel, ok := <-selections:
			if !ok {
				// The selection subscription closes only on streamCtx
				// cancellation, which the publication arm handles too; loop and let the
				// Done case end the stream cleanly.
				selections = nil
				continue
			}
			if sel == nil {
				log.Error(rpc, "the selection topic raised an empty selection; it was not sent", nil)
				continue
			}
			if err := out.Send(&agentreplv1.WatchFeedResponse{Selection: sel.selection}); err != nil {
				log.Debug(rpc, "the feed tail's client went away", dlog.Context{"cause": err.Error()})
				return nil
			}
		case sc, ok := <-scales:
			if !ok {
				// The scale subscription closes only on streamCtx cancellation,
				// which the Done case handles; loop and let it end cleanly.
				scales = nil
				continue
			}
			if sc == nil {
				log.Error(rpc, "the feed-text-scale topic raised an empty scale; it was not sent", nil)
				continue
			}
			if err := out.Send(&agentreplv1.WatchFeedResponse{FeedTextScale: sc}); err != nil {
				log.Debug(rpc, "the feed tail's client went away", dlog.Context{"cause": err.Error()})
				return nil
			}
		case overlay, ok := <-overlays:
			if !ok {
				// The overlay subscription closes only on streamCtx
				// cancellation, which the Done case handles.
				overlays = nil
				continue
			}
			if overlay == nil {
				log.Error(rpc, "the image overlay topic raised an empty overlay; it was not sent", nil)
				continue
			}
			if err := out.Send(&agentreplv1.WatchFeedResponse{ImageOverlay: overlay}); err != nil {
				log.Debug(rpc, "the feed tail's client went away", dlog.Context{"cause": err.Error()})
				return nil
			}
		}
	}
}

// GetFeedPage answers one page of a feed's history: `first` the newest page,
// `older_than` the page of rows sorting before the reader's oldest held row.
func (s *server) GetFeedPage(
	ctx context.Context,
	req *connect.Request[agentreplv1.GetFeedPageRequest],
) (*connect.Response[agentreplv1.GetFeedPageResponse], error) {
	const rpc = "GetFeedPage"
	if err := validateGetFeedPageRequest(req.Msg); err != nil {
		return nil, err
	}
	resp := &agentreplv1.GetFeedPageResponse{}
	subject, r, err := s.resolveRef(ctx, rpc, req.Msg.GetWorkspace())
	if err != nil {
		return nil, fail(s.log, rpc, err)
	}
	if r != nil {
		return answer(resp, s.refuse(s.log, rpc, resp, *r))
	}
	target, feedRefusal := feedOf(subject.Record.ID, req.Msg.Feed)
	if feedRefusal != nil {
		return answer(resp, s.refuse(subject.Log, rpc, resp, s.fill(*feedRefusal)))
	}

	var page *frontendv1.FeedPage
	if older := req.Msg.GetOlderThan(); older != nil {
		page, err = s.deps.Feed.OlderPage(ctx, subject.Record.ID, target, older.GetOldestHeld().GetKey())
	} else {
		page, _, _, err = s.deps.Feed.OpenPage(ctx, subject.Record.ID, target)
	}
	if err != nil {
		if refused, ok := s.asRefusal(err); ok {
			return answer(resp, s.refuse(subject.Log, rpc, resp, refused))
		}
		return nil, fail(subject.Log, rpc, err)
	}
	subject.Log.Debug("daemon.server.get_feed_page", "answered a history page",
		dlog.Context{"rows": len(page.GetSuccess().GetRows())})
	resp.Result = &agentreplv1.GetFeedPageResponse_Success{Success: page}
	return connect.NewResponse(resp), nil
}

// feedFrame is one publication of the feed resolver as the WatchFeed frame that
// carries it: a row frame, or a held-section frame. False is a publication
// carrying neither, which is not sent.
func feedFrame(pub feed.Publication) (*agentreplv1.WatchFeedResponse, bool) {
	switch {
	case pub.Row != nil:
		return &agentreplv1.WatchFeedResponse{Row: pub.Row}, true
	case pub.Held != nil:
		return &agentreplv1.WatchFeedResponse{Held: pub.Held}, true
	}
	return nil, false
}
