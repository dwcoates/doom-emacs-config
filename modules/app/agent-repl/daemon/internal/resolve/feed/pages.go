package feed

import (
	"context"
	"errors"
	"sort"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/feedid"
	"claude-repld/internal/ids"
)

// The page walk is PER READER and EPHEMERAL: keyed to the open connection,
// dropped at every open and at CloseReader, never persisted. A fresh or
// re-attached webview lands at the tail and pages back.
//
// A PAGE IS THE STORE'S PAGE (book.go): the rows from one loaded page's bound
// up to the next. What a walk asks for that the daemon does not hold is read
// on the spot, daemon → shim → store; what it holds (an earlier reader's
// load) is served from memory.

// OpenPage answers OpenFeed: the newest page of one feed, plus the watch token
// the tail echoes. The feed's newest store page is read first unless it is
// held and no entry has moved it since.
func (r *resolver) OpenPage(ctx context.Context, ws ids.WorkspaceID, feed feedid.Feed, reader ReaderID) (*frontendv1.FeedPage, *agentreplv1.FeedWatchToken, error) {
	return r.walkPage(ctx, ws, feed, reader, true)
}

// NextPage answers GetFeedPage's next arm: the page before this reader's
// current position, read on the spot when the daemon does not hold it.
func (r *resolver) NextPage(ctx context.Context, ws ids.WorkspaceID, feed feedid.Feed, reader ReaderID) (*frontendv1.FeedPage, error) {
	page, _, err := r.walkPage(ctx, ws, feed, reader, false)
	return page, err
}

// walkPage serves one page of a walk — its opening (OPEN) or the next older —
// loading store pages until the page it owes holds a row or nothing older
// remains. Loads of one workspace are serialized (loadMu) and read off the
// resolver's mutex; the page itself is composed, and an opening's token
// minted, under it, so no row falls between the page and its tail.
func (r *resolver) walkPage(ctx context.Context, ws ids.WorkspaceID, feed feedid.Feed, reader ReaderID, open bool) (*frontendv1.FeedPage, *agentreplv1.FeedWatchToken, error) {
	mu := r.loadMu(ws)
	mu.Lock()
	defer mu.Unlock()
	step := pageStep{open: open}
	for {
		r.mu.Lock()
		page, token, plan, err := r.stepPage(ws, feed, reader, &step)
		r.mu.Unlock()
		if err != nil || plan == nil {
			return page, token, err
		}
		if _, err := r.load(ctx, *plan); err != nil {
			if !errors.Is(err, ErrNoHistorySource) {
				return nil, nil, err
			}
			step.noSource = true
			if plan.newest {
				r.awaitSource(*plan)
			}
		}
	}
}

// pageStep is what one walkPage has done so far across its loads.
type pageStep struct {
	// open reports an opening: the walk is begun anew, at the top.
	open bool
	// newestRead reports that an opening has already decided whether the
	// newest page needed reading.
	newestRead bool
	// noSource reports that no session was up to read from: what is held is
	// all there is for now.
	noSource bool
	// exhausted is set by a step that found nothing older to serve or load:
	// the walk stands at the feed's start.
	exhausted bool
}

// stepPage composes the page a walk owes, or answers the load it needs first.
// Called with r.mu held.
func (r *resolver) stepPage(ws ids.WorkspaceID, feed feedid.Feed, reader ReaderID, step *pageStep) (*frontendv1.FeedPage, *agentreplv1.FeedWatchToken, *loadPlan, error) {
	s := r.state(ws)
	f := r.feed(s, feed)
	log := r.logger(ws)
	if step.open && !step.newestRead {
		step.newestRead = true
		if plan, ok := r.planOpening(s, f); ok && !step.noSource {
			return nil, nil, &plan, nil
		}
	}

	w := &walk{feedKey: f.key, standing: true, top: true}
	if !step.open {
		held, ok := s.readers[reader]
		if !ok || !held.standing || held.feedKey != f.key {
			// A `next` with no walk standing is a REFUSAL, not an empty page:
			// the reader is asking to continue a walk it never began.
			log.Warn("daemon.feed.next_without_walk",
				"a reader asked for the next page with no walk standing on this feed",
				dlog.Context{"feed": f.key, "reader": string(reader)})
			return nil, nil, nil, ErrNoWalk
		}
		w = held
	}
	action := "next_page"
	if step.open {
		action = "open_page"
	}
	durable, bounded := r.deliverable(s, f, action)
	end := w.end(f, durable)
	// MORE IS LOADABLE while the feed is an agent's book with a source, its
	// start was not reached, and no separation bounds delivery: a cut withholds
	// everything before it, so nothing older could ever be served.
	plan, loadable := r.planLoad(s, f, false)
	loadable = loadable && !bounded && !step.noSource
	if end <= 0 {
		if loadable {
			return nil, nil, &plan, nil
		}
		step.exhausted = true
		log.Info("daemon.feed.next_page_nothing_older",
			"a reader asked for an older page and there is nothing older: the walk stands at the feed's start",
			dlog.Context{"feed": f.key, "reader": string(reader), "bounded_by_separation": bounded, "no_source": step.noSource})
		return r.servePage(s, f, w, reader, durable, 0, len(durable), false, step.open), r.openToken(ws, f, step.open), nil, nil
	}
	start, found := pageStart(f.book.bounds, f, durable, end)
	if !found {
		if loadable {
			return nil, nil, &plan, nil
		}
		start = 0
	}
	page := r.servePage(s, f, w, reader, durable, start, end, start > 0 || loadable, step.open)
	return page, r.openToken(ws, f, step.open), nil, nil
}

// servePage composes the page ORDER[start:end), moves the walk to it, and
// records it. An opening's walk replaces whatever the reader had.
func (r *resolver) servePage(s *wsState, f *feedState, w *walk, reader ReaderID, order []string, start, end int, more, open bool) *frontendv1.FeedPage {
	page := r.composePageRange(s, f, order, start, end, more)
	if start < end {
		w.oldest = servedFrom(f, order, start)
		w.top = false
	} else if w.top {
		w.top = false
	}
	if open {
		s.readers[reader] = w
	}
	op, sentence := "daemon.feed.next_page", "a reader was served the next older page"
	if open {
		op, sentence = "daemon.feed.open_page", "a reader was served a feed's newest page and its tail was pinned"
	}
	r.logger(s.id).Debug(op, sentence, dlog.Context{
		"feed": f.key, "reader": string(reader), "rows": end - start,
		"at_start": !more, "pinned_after": f.seq, "loaded_pages": len(f.book.bounds),
	})
	return page
}

// openToken mints an opening's watch token, nil for any other step.
func (r *resolver) openToken(ws ids.WorkspaceID, f *feedState, open bool) *agentreplv1.FeedWatchToken {
	if !open {
		return nil
	}
	return r.mintToken(ws, f)
}

// end is where the next page a walk owes ends in ORDER: everything for a walk
// at the top, everything older than its oldest row otherwise.
func (w *walk) end(f *feedState, order []string) int {
	if w.top {
		return len(order)
	}
	return olderThan(f, order, w.oldest)
}

// pageStart is where the page ending at END begins: the newest loaded page's
// bound with a row of ORDER before END at or above it. False when no loaded
// page reaches below END: the rows there are held without a page of their own
// (drawn live, or by the daemon) until an older page is loaded.
func pageStart(bounds []string, f *feedState, order []string, end int) (int, bool) {
	for i := len(bounds) - 1; i >= 0; i-- {
		at := sort.Search(len(order), func(j int) bool { return f.rank[order[j]].key >= bounds[i] })
		if at < end {
			return at, true
		}
	}
	return 0, false
}

// servedFrom is the walk position after serving ORDER from START: the order
// key of the oldest row served, or none when nothing was.
func servedFrom(f *feedState, order []string, start int) *string {
	if start >= len(order) {
		return nil
	}
	key := f.rank[order[start]].key
	return &key
}

// olderThan is how many rows of ORDER sort before the walk position OLDEST:
// the end of the next older page. A walk that was served nothing stands at the
// feed's start.
func olderThan(f *feedState, order []string, oldest *string) int {
	if oldest == nil {
		return 0
	}
	return sort.Search(len(order), func(i int) bool { return f.rank[order[i]].key >= *oldest })
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
// WHAT THE BOUND HIDES IS CONVERSATION ORDER, NOT ARRIVAL. A cut can arrive
// on the live plane after a reconnect replayed the conversation that FOLLOWED
// it, and a pre-cut row can be forwarded after the cut: the rows' order keys
// are their conversation places (order.go), so both land on the right side of
// the cut whatever their arrival. `boundHides` states the rule.
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
// withheld by it. THE ORDER KEY IS CONVERSATION ORDER (order.go): a row keyed
// before the cut is pre-cut — however late it arrived, which is the "/clear
// dumped all previous history" case of a file-plane row forwarded after the
// cut — and a row keyed after it is post-cut, however early it was drawn,
// which is a reconnect's replayed post-cut conversation, kept so the bound
// never blanks the feed. The past a fork inherited sorts before every cut the
// fork makes by its class, so the fork's cut hides it.
//
// THE ONE EXCEPTION: a fork's PORTED conversation is never hidden by a cut in
// its INHERITED past. The ported prompts are drawn once, above the store's copy
// of the parent's conversation, and a cut inside that copy is a statement
// about the copy.
func boundHides(row, bound rowRank) bool {
	if row.plane == planePorted && bound.plane == planeInherited {
		return false
	}
	return row.before(bound)
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

// composePageRange renders one page: oldest → newest within the page, the edge
// arm (MORE: older rows remain to be served or loaded), and the crumbs above it.
func (r *resolver) composePageRange(s *wsState, f *feedState, order []string, start, end int, more bool) *frontendv1.FeedPage {
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
	if more {
		r.logger(s.id).Debug("daemon.feed.row_decision", "selected a feed row condition", dlog.Context{"function": "feed", "condition": "more"})
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

// awaitSource marks the book PLAN read as wanted by a reader with no session
// up to read it from: the reader is served what is held, and the page it lacks
// is loaded and pushed to it the moment a watch of the session opens
// (kickWaitingReaders).
func (r *resolver) awaitSource(plan loadPlan) {
	r.mu.Lock()
	defer r.mu.Unlock()
	if r.workspaces[plan.state.id] != plan.state {
		return
	}
	r.feed(plan.state, plan.addr).book.awaitingSource = true
}
