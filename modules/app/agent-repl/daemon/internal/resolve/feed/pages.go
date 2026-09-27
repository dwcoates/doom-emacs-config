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

	durable, _ := r.deliverable(s, f, "open_page")
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

	durable, bounded := r.deliverable(s, f, "next_page")
	if w.oldest > len(durable) {
		// THE BOUND MOVED UNDER THE WALK. A separation drawn between this
		// reader's last page and this one shortened the delivered order, and a
		// walk index taken against the longer one now points past its end. The
		// reader is at the start of what is delivered, which is what the bound
		// says it should see.
		log.Info("daemon.feed.walk_clamped_to_bound",
			"a walk stood past the delivered order after a separation moved the feed's start, so it was clamped to it",
			dlog.Context{"feed": f.key, "reader": string(reader), "stood_at": w.oldest, "rows": len(durable)})
		w.oldest = len(durable)
	}
	if w.oldest <= 0 {
		if f.historyMore != nil && !bounded {
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
		log.Info("daemon.feed.next_page_nothing_older",
			"a reader asked for an older page and there is nothing older: the walk stands at the feed's start",
			dlog.Context{"feed": f.key, "reader": string(reader), "bounded_by_separation": bounded})
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

// THE FEED BEGINS AT THE NEWEST SEPARATION.
//
// A compaction or a clear is the session saying that what came before it is no
// longer the conversation: after a compaction the surviving account IS the
// summary the divider carries, and after a clear there is no surviving account
// at all. Delivering the rows above such a divider offers a reader a
// conversation the agent no longer has — and, with two compactions in a row,
// draws the older divider and its now-superseded summary above the newer one.
//
// So the bound is a DELIVERY bound and nothing more. No row is retired, no
// feedid is minted twice, and the store's book keeps every pointer it had: the
// rows above the newest separation are simply not served in a page and not
// walked back to. Moving the bound is therefore always safe — a separation
// drawn later just makes the delivered order shorter.
//
// WHICH SEPARATIONS BOUND. Only the two that cut context: `cleared` and
// `compacted`. A `compaction_failed` divider cut NOTHING — that is the whole
// of what it says — and the worktree arms change no context at all, so neither
// may hide the conversation behind it.
//
// It is computed from the order rather than remembered as a pointer because a
// history replay and the live plane draw into the same feed by different
// routes: the NEWEST bounding separation is a fact about the row order, and
// reading it off the order cannot disagree with the order.
//
// WHAT THE BOUND HIDES IS NOT SIMPLY "EVERYTHING ABOVE IT IN THE ORDER." A cut
// arrives on the LIVE plane while the rows it does and does not bound were drawn
// on the HISTORY plane, and plane order alone reads every history row as older
// than a live cut. That blanks the feed on a reconnect: the compaction's
// POST-cut conversation is replayed in the history plane BEFORE the cut lands
// live, so by plane order it sorted above the cut and every last row of it was
// withheld — the feed went empty but for the divider though the whole
// conversation was on screen a moment before. `boundHides` reads the seam the
// plane order misses; see it for the rule.
func (r *resolver) deliverable(s *wsState, f *feedState, action string) ([]string, bool) {
	durable := durableOrder(f)
	at := boundIndex(f, durable)
	if at < 0 {
		return durable, false
	}
	boundRank := f.rank[durable[at]]
	delivered := make([]string, 0, len(durable))
	withheld := 0
	for _, id := range durable {
		if boundHides(f.rank[id], boundRank) {
			withheld++
			continue
		}
		delivered = append(delivered, id)
	}
	if withheld > 0 {
		r.logger(s.id).Info("daemon.feed.bound_at_separation",
			"the feed's delivery begins at its newest separation; the rows before it were not served",
			dlog.Context{
				"feed": f.key, "action": action, "row": durable[at],
				"withheld": withheld, "delivered": len(delivered),
			})
	}
	return delivered, true
}

// boundHides reports whether a row is the conversation BEFORE the cut, and so is
// withheld by it. Draw order — (plane, seq) — is the proxy for conversation
// order, and it is exact WITHIN a plane: a row in the cut's own plane drawn
// before it is pre-cut. ACROSS planes the proxy fails, because a cut lands on
// the live plane while the rows around it were drawn on the history plane, so
// every history row reads as older than the cut. The publication seq — ONE
// monotonic counter across every plane, fixed at a row's first draw — resolves
// what the plane cannot: an earlier-plane row drawn AFTER the cut arrived is a
// late-forwarded pre-cut row (the "/clear dumped all previous history" case) and
// is hidden; one drawn BEFORE the cut arrived is content the reader already had
// on screen (a reconnect's replayed post-cut conversation) and is kept, so the
// bound move never blanks the feed. A later-plane row is unambiguously after the
// cut and is always kept.
//
// THE PAST A FORK INHERITED PRECEDES EVERY CUT THE FORK MAKES. A ported or
// inherited row is the parent's conversation, older than anything the fork
// produced by construction (lineage.go), so a cut in any of the fork's own
// planes hides it however late it was drawn: there is no reconnect ambiguity
// to resolve, because conversation order is known.
func boundHides(row, bound rowRank) bool {
	if row.plane == bound.plane {
		return row.seq < bound.seq
	}
	if row.plane > bound.plane {
		return false
	}
	if row.plane.inheritedPast() && !bound.plane.inheritedPast() {
		return true
	}
	return row.seq > bound.seq
}

// boundIndex is the index in ORDER of the newest separation delivery begins
// at, or -1 when this feed has none.
func boundIndex(f *feedState, order []string) int {
	for i := len(order) - 1; i >= 0; i-- {
		if boundsDelivery(f.rows[order[i]]) {
			return i
		}
	}
	return -1
}

// boundsDelivery reports whether a row is a separation that CUT CONTEXT.
func boundsDelivery(row *frontendv1.FeedRow) bool {
	separation := row.GetSeparation()
	if separation == nil {
		return false
	}
	switch separation.GetKind().(type) {
	case *frontendv1.FeedSessionSeparation_Cleared, *frontendv1.FeedSessionSeparation_Compacted:
		return true
	}
	return false
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
