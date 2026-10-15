package feed

import (
	"context"
	"errors"
	"sort"

	agentreplv1 "agentrepl/proto/agentrepl/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/feedid"
	"agent-repld/internal/ids"
)

// THE READER HOLDS THE WALK'S POSITION. An opening serves the newest page; an
// older page is the page of rows sorting before the oldest row the reader
// names as held (GetFeedPage `older_than`). The daemon keeps no walk between
// requests, so a reader that frees rows, repaints, or asks on another
// connection is always answered the rows directly above what it holds.
//
// A PAGE IS THE STORE'S PAGE (book.go): the rows from one loaded page's bound
// up to the next. What a walk asks for that the daemon does not hold is read
// on the spot, daemon → shim → store; what it holds (an earlier reader's
// load) is served from memory.

// OpenPage answers OpenFeed: the newest page of one feed, the watch token the
// tail echoes, and — for the root feed only — the held section as it stands at
// that token's pin (nil for a sub-feed). The feed's newest store page is read
// first unless it is held and no entry has moved it since.
func (r *resolver) OpenPage(ctx context.Context, ws ids.WorkspaceID, feed feedid.Feed) (*frontendv1.FeedPage, *agentreplv1.FeedWatchToken, *frontendv1.FeedHeld, error) {
	return r.walkPage(ctx, ws, feed, &walk{top: true}, true)
}

// OlderPage answers GetFeedPage's older_than arm: the page of rows sorting
// before OLDESTHELD, the order key of the oldest row the reader holds, read on
// the spot when the daemon does not hold it.
func (r *resolver) OlderPage(ctx context.Context, ws ids.WorkspaceID, feed feedid.Feed, oldestHeld string) (*frontendv1.FeedPage, error) {
	page, _, _, err := r.walkPage(ctx, ws, feed, &walk{oldest: &oldestHeld}, false)
	return page, err
}

// walkPage serves one page of a walk — its opening (OPEN) or the next older —
// loading store pages until the page it owes holds a row or nothing older
// remains. Loads of one workspace are serialized (loadMu) and read off the
// resolver's mutex; the page itself is composed, and an opening's token
// minted, under it, so no row falls between the page and its tail.
func (r *resolver) walkPage(ctx context.Context, ws ids.WorkspaceID, feed feedid.Feed, w *walk, open bool) (*frontendv1.FeedPage, *agentreplv1.FeedWatchToken, *frontendv1.FeedHeld, error) {
	mu := r.loadMu(ws)
	mu.Lock()
	defer mu.Unlock()
	step := pageStep{open: open}
	for {
		r.mu.Lock()
		page, token, plan, err := r.stepPage(ws, feed, w, &step)
		// THE HELD SECTION IS READ UNDER THE LOCK THAT PINNED THE TOKEN, so it is
		// the section as of the pin, and the tail delivers every change after it.
		var held *frontendv1.FeedHeld
		if err == nil && plan == nil && token != nil && feed.Root {
			held = r.currentHeld(r.state(ws))
		}
		r.mu.Unlock()
		if err != nil || plan == nil {
			if err == nil && open {
				r.reportOpeningReads(ws, feed, step.reads)
			}
			return page, token, held, err
		}
		step.reads++
		if _, err := r.load(ctx, *plan); err != nil {
			if !errors.Is(err, ErrNoHistorySource) {
				return nil, nil, nil, err
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
	// unsourced is set by a step that served nothing older because no source
	// was up to read it from: whether older history exists is NOT KNOWN, so the
	// walk is not at the feed's start and stays open for the reader to continue.
	unsourced bool
	// reads counts the store pages the walk read.
	reads int
}

// reportOpeningReads says how many store pages an opening read before it was
// served: none (served from memory), ONE (a page whose turn heads completed
// it, or one that drew whole), or more (a page without heads whose rows waited
// on an older page's prompts, read the old way).
func (r *resolver) reportOpeningReads(ws ids.WorkspaceID, feed feedid.Feed, reads int) {
	log := r.lockedLogger(ws)
	fields := dlog.Context{"feed": r.feedKey(ws, feed), "reads": reads}
	switch {
	case reads == 0:
		log.Debug("daemon.feed.opening_reads", "an opening was served from memory without reading a store page", fields)
	case reads == 1:
		log.Info("daemon.feed.opening_reads", "an opening was served from one store read", fields)
	default:
		log.Info("daemon.feed.opening_reads", "an opening read several store pages before a row drew: its newest page's turns had their prompts on older pages and it carried no turn heads", fields)
	}
}

// stepPage composes the page walk W owes, or answers the load it needs first;
// serving moves W to the oldest row served. Called with r.mu held.
func (r *resolver) stepPage(ws ids.WorkspaceID, feed feedid.Feed, w *walk, step *pageStep) (*frontendv1.FeedPage, *agentreplv1.FeedWatchToken, *loadPlan, error) {
	s := r.state(ws)
	f := r.feed(s, feed)
	log := r.logger(ws)
	if step.open && !step.newestRead {
		step.newestRead = true
		if plan, ok := r.planOpening(s, f); ok && !step.noSource {
			return nil, nil, &plan, nil
		}
	}

	action := "next_page"
	if step.open {
		action = "open_page"
	}
	durable, bounded := r.deliverable(s, f, action)
	end := w.end(f, durable)
	// OLDER HISTORY MAY EXIST while the feed is an agent's book, its start was
	// not reached, and no separation bounds delivery: a cut withholds
	// everything before it, so nothing older could ever be served. It is
	// LOADABLE when, besides, a source is up to read it from.
	plan, older := r.planLoad(s, f, false)
	older = older && !bounded
	loadable := older && !step.noSource
	if end <= 0 {
		if loadable {
			return nil, nil, &plan, nil
		}
		if older {
			// NO SOURCE IS NOT THE FEED'S START. With no shim up nothing says
			// whether older history exists, so the page claims more and the walk
			// stays open: the reader asks again, and a source coming up loads
			// the page it is waiting on (awaitSource).
			step.unsourced = true
			log.Info("daemon.feed.next_page_no_source",
				"a reader asked for an older page and no source is up to read it from; the walk stays open and is served nothing older yet",
				dlog.Context{"feed": f.key, "open": step.open})
			return r.servePage(s, f, w, durable, 0, 0, true, step.open), r.openToken(ws, f, step.open), nil, nil
		}
		step.exhausted = true
		log.Info("daemon.feed.next_page_nothing_older",
			"a reader asked for an older page and there is nothing older: the walk stands at the feed's start",
			dlog.Context{"feed": f.key, "bounded_by_separation": bounded, "no_source": step.noSource})
		return r.servePage(s, f, w, durable, 0, len(durable), false, step.open), r.openToken(ws, f, step.open), nil, nil
	}
	start, bound, found := pageStart(f.book.bounds, f, durable, end)
	if gap, ok := r.planGap(s, f, durable, end, bound, found); ok && !bounded && !step.noSource {
		return nil, nil, &gap, nil
	}
	// AN OPENING STARTS AT THE NEWEST CUT (see `deliverable`): a compaction's
	// divider is where a fresh reader begins, and the rows above it are the
	// walk's to page back to, so the page claims more whenever any exist.
	if step.open {
		if at := cutIndex(f, durable[:end]); at >= 0 && (!found || at > start) {
			log.Debug("daemon.feed.open_at_cut",
				"an opening was served from the feed's newest cut; the rows above it are left to the walk",
				dlog.Context{"feed": f.key, "row": durable[at], "above": at, "older": older})
			return r.servePage(s, f, w, durable, at, end, at > 0 || older, true), r.openToken(ws, f, true), nil, nil
		}
	}
	if !found {
		if loadable {
			return nil, nil, &plan, nil
		}
		start = 0
	}
	page := r.servePage(s, f, w, durable, start, end, start > 0 || older, step.open)
	return page, r.openToken(ws, f, step.open), nil, nil
}

// servePage composes the page ORDER[start:end) and moves the walk to it.
func (r *resolver) servePage(s *wsState, f *feedState, w *walk, order []string, start, end int, more, open bool) *frontendv1.FeedPage {
	page := r.composePageRange(s, f, order, start, end, more)
	// A WALK SERVED NOTHING STANDS AT THE FEED'S START only when the page says
	// so; one served nothing while older history may exist (no source yet)
	// stays above every row, so its next page is the newest held.
	if start < end {
		w.oldest = servedFrom(f, order, start)
		w.top = false
	} else if w.top && !more {
		w.top = false
	}
	op, sentence := "daemon.feed.next_page", "a reader was served the next older page"
	if open {
		op, sentence = "daemon.feed.open_page", "a reader was served a feed's newest page and its tail was pinned"
	}
	r.logger(s.id).Debug(op, sentence, dlog.Context{
		"feed": f.key, "rows": end - start,
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
func pageStart(bounds []string, f *feedState, order []string, end int) (int, string, bool) {
	for i := len(bounds) - 1; i >= 0; i-- {
		at := sort.Search(len(order), func(j int) bool { return f.rank[order[j]].key >= bounds[i] })
		if at < end {
			return at, bounds[i], true
		}
	}
	return 0, "", false
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

// THE FEED OPENS AT ITS NEWEST CUT, AND ITS HISTORY ENDS AT ITS NEWEST CLEAR.
//
// A compaction or a clear is the session saying that what came before it is no
// longer the conversation. The two differ in what a reader may still see:
//
//   - A CLEAR ENDS THE FEED'S HISTORY. Nothing survived it, so the rows above
//     the newest `cleared` divider are never served in a page, never walked
//     back to, and never pushed. That is the DELIVERY bound this function
//     applies.
//   - A COMPACTION ONLY MOVES WHERE THE FEED OPENS (owner ruling, 2026-10-10).
//     A fresh opening starts at the newest `compacted` divider and shows
//     nothing above it, but the page claims more, so a reader can page back
//     through what happened before the compaction, down to the newest clear.
//     The rows above it are still kept off the live push (pushWithheldByBound):
//     they reach a reader only through the walk.
//
// So the bound is a DELIVERY bound and nothing more. No row is retired, no
// feedid is minted twice, and the store's book keeps every pointer it had: the
// rows above the newest clear are simply not served in a page and not walked
// back to. Moving the bound is therefore always safe — a clear drawn later
// just makes the delivered order shorter.
//
// WHICH SEPARATIONS COUNT. Only the two that cut context: `cleared` and
// `compacted`. A `compaction_failed` divider cut NOTHING — that is the whole
// of what it says — and the worktree arms change no context at all, so neither
// may hide the conversation behind it nor move where the feed opens.
//
// Both are computed from the order rather than remembered as a pointer because
// a history replay and the live plane draw into the same feed by different
// routes: the NEWEST cut is a fact about the row order, and reading it off the
// order cannot disagree with the order.
//
// WHAT THE BOUND HIDES IS CONVERSATION ORDER, NOT ARRIVAL. A cut can arrive
// on the live plane after a reconnect replayed the conversation that FOLLOWED
// it, and a pre-cut row can be forwarded after the cut: the rows' order keys
// are their conversation places (order.go), so both land on the right side of
// the cut whatever their arrival. `boundHides` states the rule.
func (r *resolver) deliverable(s *wsState, f *feedState, action string) ([]string, bool) {
	durable := durableOrder(f)
	at := historyEndIndex(f, durable)
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
			"the feed's delivery begins at its newest clear; the rows before it were not served",
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
func boundHides(row, bound rowRank) bool {
	return row.before(bound)
}

// cutIndex is the index in ORDER of the newest separation that cut context (a
// clear or a compaction), or -1 when ORDER has none: where an opening starts,
// and the rows above which the live push withholds.
func cutIndex(f *feedState, order []string) int {
	return newestIndex(f, order, cutsContext)
}

// historyEndIndex is the index in ORDER of the newest clear, or -1 when ORDER
// has none: the delivery bound, above which nothing is served or walked to.
func historyEndIndex(f *feedState, order []string) int {
	return newestIndex(f, order, endsHistory)
}

// newestIndex is the index in ORDER of the newest row IS reports, or -1.
func newestIndex(f *feedState, order []string, is func(*frontendv1.FeedRow) bool) int {
	for i := len(order) - 1; i >= 0; i-- {
		if is(f.rows[order[i]]) {
			return i
		}
	}
	return -1
}

// cutsContext reports whether a row is a separation that CUT CONTEXT.
func cutsContext(row *frontendv1.FeedRow) bool {
	switch row.GetSeparation().GetKind().(type) {
	case *frontendv1.FeedSessionSeparation_Cleared, *frontendv1.FeedSessionSeparation_Compacted:
		return true
	}
	return false
}

// endsHistory reports whether a row is a separation the feed's history ends
// at: a clear, which nothing survived.
func endsHistory(row *frontendv1.FeedRow) bool {
	_, cleared := row.GetSeparation().GetKind().(*frontendv1.FeedSessionSeparation_Cleared)
	return cleared
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

// awaitSource marks the book PLAN read as wanted by a reader with no source up
// to read it from: the reader is served what is held, and the page it lacks is
// loaded and pushed to it the moment a source comes up (SourceUp) or a watch of
// the session opens (kickWaitingReaders).
func (r *resolver) awaitSource(plan loadPlan) {
	r.mu.Lock()
	defer r.mu.Unlock()
	if r.workspaces[plan.state.id] != plan.state {
		return
	}
	r.feed(plan.state, plan.addr).book.awaitingSource = true
}

// planGap answers the store page a walk must read before serving the page that
// ends at END and begins at BOUND (none when not FOUND), when that page reaches
// into the unpaged gap a re-read newest page left (bookState.gapAfter): rows
// above the gap's floor that no store page bounds. Called with r.mu held.
func (r *resolver) planGap(s *wsState, f *feedState, order []string, end int, bound string, found bool) (loadPlan, bool) {
	if f.book.gapAfter == nil || r.deps.History == nil {
		return loadPlan{}, false
	}
	floor := sort.Search(len(order), func(i int) bool { return f.rank[order[i]].key >= f.book.gapFloor })
	if floor >= end || (found && bound > f.book.gapFloor) {
		return loadPlan{}, false
	}
	target, _ := bookTarget(s.feedAddrs[f.key])
	return loadPlan{addr: s.feedAddrs[f.key], state: s, target: target, after: f.book.gapAfter, gap: true}, true
}
