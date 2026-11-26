package server

import (
	"context"
	"errors"
	"fmt"
	"time"

	"connectrpc.com/connect"

	"agentrepl/logging/logsite"
	agentreplv1 "agentrepl/proto/agentrepl/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	workspacev1 "agentrepl/proto/workspace/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/feedid"
	"agent-repld/internal/resolve/feed"
)

// The feed's three rpcs. OpenFeed mints the watch token and answers the newest
// page; WatchFeed tails from exactly that token's pin; GetFeedPage answers the
// newest page again, or the page above the oldest row the reader names as
// held. The daemon keeps no walk between requests: the reader's oldest held
// row is the position.

// feedSubject is the feed a request's FeedId addresses and the workspace that
// owns it.
type feedSubject struct {
	// Owner is the workspace whose feed it is: the request's own workspace, or
	// a workspace beneath it (a child bubble's sub-feed is the child's own root
	// feed, feedid.KindChild).
	Owner resolved
	// Feed is the feed within Owner.
	Feed feedid.Feed
}

// resolveFeed resolves an optional FeedId onto the feed it addresses, for a
// request on VIEWER's page. An UNSET id is the viewer's ROOT feed, the only
// feed every workspace always has. It refuses an undecodable id, and a feed
// whose owner is neither the viewer nor beneath it; an owner beneath the
// viewer is resolved as any request's workspace is (resolveRef), so one this
// daemon does not serve, or no longer registered, is refused as itself. An
// error is the caller's to answer through failResolution, which records it.
func (s *server) resolveFeed(ctx context.Context, rpc string, viewer resolved, id *frontendv1.FeedId) (feedSubject, *refusal, error) {
	if id == nil {
		return feedSubject{Owner: viewer, Feed: feedid.Feed{Root: true}}, nil, nil
	}
	owner, decoded, err := feedid.DecodeFeed(id)
	if err != nil {
		r := s.fill(refusal{
			Arm:    "feed_undecodable",
			Reason: fmt.Sprintf("the feed id %q does not decode: %v", id.GetValue(), err),
		})
		return feedSubject{}, &r, nil
	}
	if owner == viewer.Record.ID {
		return feedSubject{Owner: viewer, Feed: decoded}, nil, nil
	}
	ownerSubject, r, err := s.resolveRef(ctx, rpc, &workspacev1.WorkspaceRef{Id: string(owner)})
	if err != nil || r != nil {
		return feedSubject{}, r, err
	}
	beneath, ended, err := readRegistryValue(s, func() (bool, error) {
		return s.deps.DB.DescendsFrom(ctx, owner, viewer.Record.ID)
	})
	if ended {
		return feedSubject{}, nil, connect.NewError(connect.CodeUnavailable,
			fmt.Errorf("%s: feed of workspace %q: %w", rpc, owner, errServingEnded))
	}
	if err != nil {
		return feedSubject{}, nil, fmt.Errorf("%s: whether the feed's workspace %q lies beneath %q: %w", rpc, owner, viewer.Record.ID, err)
	}
	if !beneath {
		r := s.fill(refusal{
			Arm: "feed_not_in_workspace",
			Reason: fmt.Sprintf("the feed id %q belongs to workspace %q, which is neither %q nor beneath it",
				id.GetValue(), owner, viewer.Record.ID),
		})
		return feedSubject{}, &r, nil
	}
	return feedSubject{Owner: ownerSubject, Feed: decoded}, nil, nil
}

// OpenFeed opens ONE feed — the root, or a bubble's sub-feed (a child
// bubble's is the child's own root feed, resolveFeed) — answering the newest page and MINTING the watch token WatchFeed echoes. The
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
	began := time.Now()
	subject, r, err := s.resolveRef(ctx, rpc, req.Msg.GetWorkspace())
	if err != nil {
		return nil, failResolution(s.log, rpc, err)
	}
	if r != nil {
		return answer(resp, s.refuse(s.log, rpc, resp, *r))
	}
	fs, feedRefusal, err := s.resolveFeed(ctx, rpc, subject, req.Msg.Feed)
	if err != nil {
		return nil, failResolution(subject.Log, rpc, err)
	}
	if feedRefusal != nil {
		return answer(resp, s.refuse(subject.Log, rpc, resp, *feedRefusal))
	}
	target := fs.Feed

	started := time.Now()
	page, token, held, err := s.deps.Feed.OpenPage(ctx, fs.Owner.Record.ID, target)
	if err != nil {
		if refused, ok := s.asRefusal(err); ok {
			return answer(resp, s.refuse(subject.Log, rpc, resp, refused))
		}
		return nil, fail(subject.Log, rpc, err)
	}
	// A CHILD'S ROOT FEED DRAWN IN ITS PARENT'S BUBBLE IS A SUB-FEED there,
	// and a sub-feed has no held section: the child's held prompts are its
	// own page's.
	watched := tokenTarget{WS: fs.Owner.Record.ID, Feed: target, Viewer: subject.Record.ID}
	if !watched.pageRoot() {
		held = nil
	}
	var resumed *agentreplv1.OpenFeedResumed
	if req.Msg.Resume != nil {
		resumed = resumePage(page, req.Msg.GetResume())
	}
	s.rememberToken(token.GetValue(), watched)
	// INFO: an open is a page's first paint waiting on the daemon, once per
	// page load; its duration is half of what the page's own feed.open-answered
	// record measures. resolve_ms is the workspace's resolution before it
	// (its registry read), which `ms` does not count.
	subject.Log.Info("daemon.server.open_feed", "opened a feed and minted its watch token",
		dlog.Context{"token": token.GetValue(), "rows": len(page.GetSuccess().GetRows()),
			"root": target.Root, "owner": string(fs.Owner.Record.ID), "held": len(held.GetItems()),
			"resumed": resumed != nil, "standing": len(resumed.GetStanding()),
			"ms": time.Since(started).Milliseconds(), "resolve_ms": started.Sub(began).Milliseconds()})
	resp.Result = &agentreplv1.OpenFeedResponse_Success{
		Success: &agentreplv1.OpenFeedSuccess{Page: page, Watch: token, Held: held, Resumed: resumed},
	}
	return connect.NewResponse(resp), nil
}

// rememberToken records which workspace and feed a minted watch token
// addresses. WatchFeedRequest carries ONLY the token, while the feed resolver's
// Tail takes the workspace and the feed explicitly, so the mint site is the one
// place that still knows both.
func (s *server) rememberToken(token string, target tokenTarget) {
	s.mu.Lock()
	defer s.mu.Unlock()
	s.watchTokens[token] = target
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
	tailGone := logsite.Msg("the feed tail's client went away")
	const rpc = "WatchFeed"
	if err := validateWatchFeedRequest(msg); err != nil {
		return err
	}
	token := msg.GetWatch()
	// A TOKEN ANOTHER DAEMON MINTED IS UNKNOWN HERE (FeedWatchToken.instance).
	// It reaches this daemon only when its minter no longer runs
	// (forward.go instanceTarget), and the client opens the feed again.
	if token.GetInstance() != string(s.deps.Instance) {
		return TransportClosed(s.log, rpc, "unknown_token",
			fmt.Sprintf("feed watch token %q was minted by daemon %q, not this one", token.GetValue(), token.GetInstance()), true)
	}
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

	// Every frame counts as work in flight until it is written (internal/inflight).
	pubs, ackPub := tail.Publications(streamCtx)
	ackSelection, ackScale, ackOverlay, ackComposerEntry, ackQuestion, ackHighlight := func() {}, func() {}, func() {}, func() {}, func() {}, func() {}

	// THE RESPONSE-SELECTION PUSH RIDES THE ROOT FEED'S WATCH ONLY. The
	// FeedSelection frame names rows of THIS feed (the selected/centered
	// final-response rows), and those rows live on the root feed; a sub-feed's
	// watch carries none, so it subscribes to no selection. A nil channel in
	// the select below simply never fires, which is what a non-root feed wants.
	var selections <-chan *selectionState
	if target.pageRoot() {
		selections, ackSelection = s.selectionTopic(target.WS).SubscribeStream(streamCtx)
	}

	// THE FEED TEXT ZOOM RIDES EVERY FEED'S WATCH. Unlike the selection push,
	// the scale is daemon-global and applies to all feed text, so a sub-feed's
	// watch subscribes too. The topic replays its latest value, so this delivers
	// the current zoom the instant the tail is accepted (Prime seeded it before
	// serving), then every later change.
	var scales <-chan *frontendv1.FeedTextScale
	scales, ackScale = s.feedTextScaleTopic.SubscribeStream(streamCtx)

	// THE IMAGE OVERLAY RIDES THE ROOT FEED'S WATCH ONLY, like the selection:
	// it lies over the workspace's root feed. The topic replays its latest
	// value, so a page opening after the image was opened draws it at once.
	var overlays <-chan *frontendv1.FeedImageOverlay
	if target.pageRoot() {
		overlays, ackOverlay = s.imageOverlayTopic(target.WS).SubscribeStream(streamCtx)
	}

	// THE COMPOSER'S ENTRY RIDES THE ROOT FEED'S WATCH ONLY, like the
	// selection: it names a row of the root feed. The topic replays its latest
	// value, so a page opening mid-edit draws the highlight that stands.
	var composerEntries <-chan *frontendv1.FeedComposerEntry
	if target.pageRoot() {
		composerEntries, ackComposerEntry = s.composerEntryTopic(target.WS).SubscribeStream(streamCtx)
	}

	// THE QUESTION ABOUT A RESPONSE RIDES THE ROOT FEED'S WATCH ONLY, like the
	// selection: it names a row of the root feed. The topic replays its latest
	// value, so a page opening while a question stands draws it.
	var questions <-chan *frontendv1.FeedResponseQuestion
	if target.pageRoot() {
		questions, ackQuestion = s.responseQuestionTopic(target.WS).SubscribeStream(streamCtx)
	}

	// THE RESPONSE HIGHLIGHT RIDES THE ROOT FEED'S WATCH ONLY: it names a row
	// of the root feed. The topic is seeded with none, so every root watch is
	// told the state the daemon holds, a just-restarted daemon's included.
	var highlights <-chan *frontendv1.FeedResponseHighlight
	if target.pageRoot() {
		highlights, ackHighlight = s.watchResponseHighlight(streamCtx, target.WS)
	}

	s.acceptStream(ctx, rpc)
	log.Debug(rpc, "accepted a feed tail", dlog.Context{"token": token.GetValue()})
	for {
		ack := func() {}
		select {
		case <-streamCtx.Done():
			log.Debug(rpc, "the feed tail ended on cancellation", nil)
			return nil
		case pub, ok := <-pubs:
			if !ok {
				log.Debug(rpc, "the feed tail's subscription closed", nil)
				return nil
			}
			ack = ackPub
			frame, sendable := feedFrame(pub)
			if !sendable {
				log.Error(rpc, "the feed resolver raised a publication carrying neither a row nor a held change; it was not sent", nil)
				ack()
				continue
			}
			if err := out.Send(frame); err != nil {
				log.Log(dlog.LevelDebug, rpc, tailGone, dlog.Context{"cause": err.Error()})
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
			ack = ackSelection
			if sel == nil {
				log.Error(rpc, "the selection topic raised an empty selection; it was not sent", nil)
				ack()
				continue
			}
			if err := out.Send(&agentreplv1.WatchFeedResponse{Selection: sel.selection}); err != nil {
				log.Log(dlog.LevelDebug, rpc, tailGone, dlog.Context{"cause": err.Error()})
				return nil
			}
		case sc, ok := <-scales:
			if !ok {
				// The scale subscription closes only on streamCtx cancellation,
				// which the Done case handles; loop and let it end cleanly.
				scales = nil
				continue
			}
			ack = ackScale
			if sc == nil {
				log.Error(rpc, "the feed-text-scale topic raised an empty scale; it was not sent", nil)
				ack()
				continue
			}
			if err := out.Send(&agentreplv1.WatchFeedResponse{FeedTextScale: sc}); err != nil {
				log.Log(dlog.LevelDebug, rpc, tailGone, dlog.Context{"cause": err.Error()})
				return nil
			}
		case overlay, ok := <-overlays:
			if !ok {
				// The overlay subscription closes only on streamCtx
				// cancellation, which the Done case handles.
				overlays = nil
				continue
			}
			ack = ackOverlay
			if overlay == nil {
				log.Error(rpc, "the image overlay topic raised an empty overlay; it was not sent", nil)
				ack()
				continue
			}
			if err := out.Send(&agentreplv1.WatchFeedResponse{ImageOverlay: overlay}); err != nil {
				log.Log(dlog.LevelDebug, rpc, tailGone, dlog.Context{"cause": err.Error()})
				return nil
			}
		case entry, ok := <-composerEntries:
			if !ok {
				// Closes only on streamCtx cancellation, which the Done case
				// handles.
				composerEntries = nil
				continue
			}
			ack = ackComposerEntry
			if entry == nil {
				log.Error(rpc, "the composer-entry topic raised an empty entry; it was not sent", nil)
				ack()
				continue
			}
			if err := out.Send(&agentreplv1.WatchFeedResponse{ComposerEntry: entry}); err != nil {
				log.Log(dlog.LevelDebug, rpc, tailGone, dlog.Context{"cause": err.Error()})
				return nil
			}
		case question, ok := <-questions:
			if !ok {
				// Closes only on streamCtx cancellation, which the Done case
				// handles.
				questions = nil
				continue
			}
			ack = ackQuestion
			if question == nil {
				log.Error(rpc, "the response-question topic raised an empty question; it was not sent", nil)
				ack()
				continue
			}
			if err := out.Send(&agentreplv1.WatchFeedResponse{ResponseQuestion: question}); err != nil {
				log.Log(dlog.LevelDebug, rpc, tailGone, dlog.Context{"cause": err.Error()})
				return nil
			}
		case highlight, ok := <-highlights:
			if !ok {
				// Closes only on streamCtx cancellation, which the Done case
				// handles.
				highlights = nil
				continue
			}
			ack = ackHighlight
			if highlight == nil {
				log.Error(rpc, "the response-highlight topic raised an empty highlight; it was not sent", nil)
				ack()
				continue
			}
			if err := out.Send(&agentreplv1.WatchFeedResponse{ResponseHighlight: highlight}); err != nil {
				log.Log(dlog.LevelDebug, rpc, tailGone, dlog.Context{"cause": err.Error()})
				return nil
			}
		}
		ack()
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
		return nil, failResolution(s.log, rpc, err)
	}
	if r != nil {
		return answer(resp, s.refuse(s.log, rpc, resp, *r))
	}
	fs, feedRefusal, err := s.resolveFeed(ctx, rpc, subject, req.Msg.Feed)
	if err != nil {
		return nil, failResolution(subject.Log, rpc, err)
	}
	if feedRefusal != nil {
		return answer(resp, s.refuse(subject.Log, rpc, resp, *feedRefusal))
	}

	var page *frontendv1.FeedPage
	if older := req.Msg.GetOlderThan(); older != nil {
		page, err = s.deps.Feed.OlderPage(ctx, fs.Owner.Record.ID, fs.Feed, older.GetOldestHeld().GetKey())
	} else {
		page, _, _, err = s.deps.Feed.OpenPage(ctx, fs.Owner.Record.ID, fs.Feed)
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
