package feed

import (
	"context"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/feedid"
	"claude-repld/internal/ids"
)

// The page walk is PER READER and EPHEMERAL: keyed to the open connection,
// dropped at every open and at CloseReader, never persisted. A fresh or
// re-attached webview lands at the tail and pages back.

// OpenPage answers OpenFeed: the newest page of one feed, plus the watch token
// the tail echoes.
func (r *resolver) OpenPage(ctx context.Context, ws ids.WorkspaceID, feed feedid.Feed, reader ReaderID) (*frontendv1.FeedPage, *agentreplv1.FeedWatchToken, error) {
	r.mu.Lock()
	defer r.mu.Unlock()
	s := r.state(ws)
	f := r.feed(s, feed)
	log := r.logger(ws)

	durable := durableOrder(f)
	start := len(durable) - r.deps.PageSize
	if start < 0 {
		r.logger(ws).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "start < 0"})
		start = 0
	}
	page := r.composePage(s, f, durable, start)
	s.readers[reader] = &walk{feedKey: f.key, oldest: start, standing: true}
	token := r.mintToken(ws, f)

	log.Debug("daemon.feed.open_page",
		"a reader was served a feed's newest page and its tail was pinned",
		dlog.Context{
			"feed": f.key, "reader": string(reader), "rows": len(durable) - start,
			"at_start": start == 0, "pinned_after": f.seq,
		})
	return page, token, nil
}

// NextPage answers GetFeedPage's next arm: the page before this reader's
// current position.
func (r *resolver) NextPage(ctx context.Context, ws ids.WorkspaceID, feed feedid.Feed, reader ReaderID) (*frontendv1.FeedPage, error) {
	r.mu.Lock()
	defer r.mu.Unlock()
	s := r.state(ws)
	f := r.feed(s, feed)
	log := r.logger(ws)

	w, ok := s.readers[reader]
	if !ok || !w.standing || w.feedKey != f.key {
		// A `next` with no walk standing is a REFUSAL, not an empty page: the
		// reader is asking to continue a walk it never began.
		log.Warn("daemon.feed.next_without_walk",
			"a reader asked for the next page with no walk standing on this feed",
			dlog.Context{"feed": f.key, "reader": string(reader)})
		return nil, ErrNoWalk
	}

	durable := durableOrder(f)
	if w.oldest <= 0 {
		if f.historyMore != nil {
			// The walk reached the oldest row the replay delivered, and the
			// record says older history exists: what is on screen has a HOLE
			// in it, and saying so is the honest answer.
			log.Warn("daemon.feed.history_replay_truncated",
				"a walk reached the oldest replayed row while older history remains",
				dlog.Context{"feed": f.key, "reader": string(reader)})
			return &frontendv1.FeedPage{Result: &frontendv1.FeedPage_Error{
				Error: &frontendv1.FeedPageError{
					Headline: &frontendv1.FeedPageErrorHeadline{
						Text: "the history replay stopped before it reached the live conversation",
						Tone: "warning",
					},
					Kind: &frontendv1.FeedPageError_HistoryReplayTruncated{HistoryReplayTruncated: f.historyMore},
				},
			}}, nil
		}
		log.Debug("daemon.feed.next_page",
			"a reader's walk is already at the feed's start",
			dlog.Context{"feed": f.key, "reader": string(reader)})
		return r.composePage(s, f, durable, 0), nil
	}

	end := w.oldest
	start := end - r.deps.PageSize
	if start < 0 {
		r.logger(ws).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "start < 0"})
		start = 0
	}
	page := r.composePageRange(s, f, durable, start, end)
	w.oldest = start

	log.Debug("daemon.feed.next_page",
		"a reader was served the next older page",
		dlog.Context{"feed": f.key, "reader": string(reader), "rows": end - start, "at_start": start == 0})
	return page, nil
}

// CloseReader drops a reader's walk when its connection ends.
func (r *resolver) CloseReader(ws ids.WorkspaceID, reader ReaderID) {
	r.mu.Lock()
	defer r.mu.Unlock()
	s := r.state(ws)
	_, had := s.readers[reader]
	delete(s.readers, reader)
	r.logger(ws).Debug("daemon.feed.close_reader",
		"a reader's page walk was dropped", dlog.Context{"reader": string(reader), "had_walk": had})
}

// durableOrder is the feed's row order with the non-durable rows removed. A
// command panel and a refusal card live in resolver memory only: they are
// pushed live and never paged, so a restart cannot make them look replayed.
func durableOrder(f *feedState) []string {
	out := make([]string, 0, len(f.order))
	for _, id := range f.order {
		if f.nonDurable[id] {
			continue
		}
		out = append(out, id)
	}
	return out
}

// composePage renders the page from start to the newest row.
func (r *resolver) composePage(s *wsState, f *feedState, order []string, start int) *frontendv1.FeedPage {
	return r.composePageRange(s, f, order, start, len(order))
}

// composePageRange renders one page: oldest → newest within the page, the edge
// arm, and the crumbs above it.
func (r *resolver) composePageRange(s *wsState, f *feedState, order []string, start, end int) *frontendv1.FeedPage {
	rows := make([]*frontendv1.FeedRow, 0, end-start)
	for _, id := range order[start:end] {
		if row, ok := f.rows[id]; ok {
			r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "row, ok := f.rows[id]; ok"})
			rows = append(rows, row)
		}
	}
	success := &frontendv1.FeedPageSuccess{
		Rows:        rows,
		Breadcrumbs: r.breadcrumbs(s, f.key),
	}
	if start > 0 {
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "start > 0"})
		success.Edge = &frontendv1.FeedPageSuccess_HasMore{HasMore: &frontendv1.FeedPageHasMore{}}
	} else {
		success.Edge = &frontendv1.FeedPageSuccess_AtStart{AtStart: &frontendv1.FeedPageAtStart{}}
	}
	return &frontendv1.FeedPage{Result: &frontendv1.FeedPage_Success{Success: success}}
}

// breadcrumbs walks a sub-feed's containment chain to the root and returns the
// crumbs OUTERMOST FIRST. The root feed's own top has none.
func (r *resolver) breadcrumbs(s *wsState, feedKey string) *frontendv1.FeedBreadcrumbs {
	var reversed []*frontendv1.FeedBreadcrumb
	key := feedKey
	for depth := 0; depth < len(s.subFeeds)+1; depth++ {
		head, ok := s.subFeeds[key]
		if !ok {
			r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "!ok"})
			break
		}
		reversed = append(reversed, &frontendv1.FeedBreadcrumb{Target: head.row, Label: head.label})
		key = head.parentFeed
	}
	crumbs := make([]*frontendv1.FeedBreadcrumb, 0, len(reversed))
	for i := len(reversed) - 1; i >= 0; i-- {
		crumbs = append(crumbs, reversed[i])
	}
	return &frontendv1.FeedBreadcrumbs{Crumbs: crumbs}
}
