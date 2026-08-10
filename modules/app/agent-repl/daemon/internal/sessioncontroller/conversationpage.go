package sessioncontroller

import (
	"context"
	"fmt"

	corev1 "agentrepl/proto/agentshim/core/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/errclass"
	"claude-repld/internal/shimclient"
	"claude-repld/internal/ssm"
)

// The BACKWARDS PAGE READER: the daemon half of tail-first conversation
// loading (conversation-page.proto).
//
// # The read it replaces
//
// A cold webview asked for its conversation with ResyncCmd{from_seq: 0} and
// received every store event the session ever produced — 259,000 events and
// 186MB in the worst workspace observed — to draw a screen whose visible tail
// is about ten items. Nothing about that replay was incorrect; it answered a
// question no client had any reason to ask.
//
// This answers the question they actually have: the newest N top-level items,
// and a handle for the N before those.
//
// # It is the SAME translation, not a second one
//
// Page items are assembled by driving store events through
// consumer.pushConversation — the one curation chokepoint every replay route
// already funnels through (sinks.go). The withhold passes, the clear/compact
// coalescing, the provenance stamp read from the merge lease's durable ledger,
// the spawned-bubble stamps: a page gets all of it, because it is literally
// the same code path. What differs is only the SINK. A page's deltas are
// captured into a buffer instead of being pushed to clients, which is what
// makes assembling one free of any visible side effect.
//
// A second, page-shaped translator was the obvious alternative and is the
// thing this exists to avoid: two curators that must be kept in agreement
// forever, whose disagreement shows up as a bubble that renders one way when
// it is live and another way when it is paged.
//
// # Where the events come from, and why the split is the resync's split
//
// The route is chosen exactly as a resync chooses it, and for exactly the same
// reason (repull.go's header, storehistory's package header):
//
//   - A workspace with a LIVE session controller is served THROUGH THE SHIM
//     (core.v1 ReplayRequest). The store is an internal of the agent-shim
//     facade, and a daemon that dials it directly while the shim is up would
//     be serving history through a side door — a fallback that masks a shim
//     outage instead of surfacing it.
//   - An UNWIRED workspace is served from DURABLE history. There is no shim,
//     nothing is broken, and the store is not a second route to the history —
//     it is the only one. Spawning a vendor process to answer a read would
//     charge a session bring-up to a frontend that merely mounted.
//
// Neither route is a backwards reader, because neither store nor shim offers
// one: both replay FORWARD from a seq. So this walks backwards in WINDOWS —
// read a bounded range ending at the anchor, translate it, and widen the range
// only if it did not yield enough items. A window is re-read when it widens,
// which costs at most twice the final window and is what buys the guarantee
// that a page never scans the whole conversation to find its tail.
//
// # What a page is bounded by
//
// The FLOOR is the resync's floor, unchanged: max(0, newest clear or
// compaction), inclusive of that event itself (replayFloorAt). History above a
// clear is history the frontend would discard, so paging into it would serve
// pages nobody can see. Reaching the floor is also what mints
// ConversationPageStart, which is why "the beginning" means the beginning of
// the LIVE conversation rather than of the seq space.

// pageDefaultLimit is what a client asking for 0 items receives.
//
// About one screen of feed. The number is the daemon's rather than the
// client's because the daemon is the only end that knows what an item costs to
// assemble, and because a default a client states is a default that drifts per
// client.
const pageDefaultLimit = 10

// pageMaxLimit is the ceiling every request is clamped to.
//
// It is a CLAMP rather than a refusal: a client asking for 5,000 items is
// asking for the full replay this whole path exists to end, and the honest
// answer to that is the largest page the daemon will serve, not an error the
// client has no way to act on. Making the ceiling unrepresentable this way is
// what keeps a paging client from re-inventing the 186MB cold open one
// oversized limit at a time.
const pageMaxLimit = 50

// pageInitialWindow is how many seqs the first backwards window spans.
//
// Seqs are dense within a session's store, so a window of N seqs is
// approximately N events. Sized so an ordinary tail page — ten items, each a
// handful of events — is served by the FIRST window and never widens.
const pageInitialWindow = 512

// pageWindowGrowth multiplies the window on each widening.
//
// Quadrupling rather than doubling: a widening means the previous window was
// wrong about event density (a conversation full of tool traffic can spend
// hundreds of events per rendered item), and creeping up on the right size a
// factor of two at a time re-reads the range far more often than it needs to.
const pageWindowGrowth = 4

// pageMaxEvents caps how many events ONE page assembly may read across all its
// windows.
//
// A page is a bounded read by contract, and this is the bound. It is reached
// only by a conversation whose events curate to almost nothing over a very
// long stretch; when it is, the page is served SHORT with a continuation
// cursor rather than being widened further, because a short page a client can
// continue is a better answer than an unbounded scan. The daemon says so in
// its log — a silently short page would read as "this is where the
// conversation thins out".
const pageMaxEvents = 20000

// PageAnchor is the resolved form of ConversationPageCmd's anchor oneof.
//
// It is a daemon-side type rather than the wire message because the wire
// message's `before` arm carries an opaque cursor this package is the only
// reader of; resolving it at the boundary means nothing below has to hold both
// the encoded and the decoded form.
type PageAnchor struct {
	// Tail selects the newest end of the conversation. Mutually exclusive with
	// Cursor, and the frontend command layer is what proves the oneof set
	// exactly one of them.
	Tail bool
	// Cursor is the opaque continuation token from a prior page's `more` arm,
	// still encoded. Empty on a tail request.
	Cursor string
	// Limit is the client's requested top-level item count, BEFORE clamping.
	Limit uint32
}

// pageRangeReader replays one bounded seq range, oldest first.
//
// The two routes a page can be served from — the shim's ReplayRequest and the
// store's own subscription — already have this exact shape, differing only in
// the result struct they report. Naming the shape once is what lets the
// windowed backwards walk below be written once rather than twice, and what
// makes "which route served this page" a decision taken at one place instead
// of threaded through every loop.
//
// fromSeq is EXCLUSIVE (matching Subscribe.from_seq and ReplayRequest.from_seq)
// and toSeq is an EXCLUSIVE upper bound, with 0 meaning "until the history
// drains".
type pageRangeReader func(ctx context.Context, fromSeq, toSeq uint64, maxEvents uint32, onEvent func(*corev1.Event)) (pageRangeResult, error)

// pageRangeResult is the common report both routes make about one range.
type pageRangeResult struct {
	Delivered uint64
	Truncated bool
	Reason    string
}

// pageCapture is the Pusher a page-assembling consumer is bound to.
//
// EVERY PLANE BUT THE FEED IS DROPPED, and each drop is a decision rather than
// an omission:
//
//   - ConversationDelta is the page's whole content, and is captured.
//   - AsyncBubbleDelta is dropped because the connect StateSnapshot already
//     carries every async bubble the session holds, folded to date
//     (StateSnapshot.async_bubbles). A page re-pushing them would re-open
//     bubbles the client already has, and the anchors that address them ride
//     the feed plane and so are captured.
//   - TypingDelta, TaskCatalog, WorkspaceState, SessionInitView, Heartbeat,
//     Queue and Progress are LIVE state. A history read that moved any of them
//     would have replayed history rewrite the present, which is the
//     replayed-history-as-live-state defect repull.go exists to avoid.
//
// It is not a silent discard: assembling a page is a read, and a read that
// changed a client's live state would be the bug.
type pageCapture struct {
	deltas []*frontendv1.ConversationDelta
}

func (p *pageCapture) PushConversationDelta(cd *frontendv1.ConversationDelta) {
	p.deltas = append(p.deltas, cd)
}

func (p *pageCapture) PushAsyncBubbleDelta(*frontendv1.AsyncBubbleDelta) {}
func (p *pageCapture) PushTypingDelta(*frontendv1.TypingDelta)           {}
func (p *pageCapture) PushTaskCatalog(*frontendv1.TaskCatalog)           {}
func (p *pageCapture) PushWorkspaceState(*frontendv1.WorkspaceState)     {}
func (p *pageCapture) PushSessionInitView(*frontendv1.SessionInitView)   {}
func (p *pageCapture) PushHeartbeatView(*frontendv1.HeartbeatView)       {}
func (p *pageCapture) PushQueueView(*frontendv1.QueueView)               {}
func (p *pageCapture) PushProgressView(*frontendv1.ProgressView)         {}

// compile-time proof the capture really is the sink a consumer accepts.
var _ Pusher = (*pageCapture)(nil)

// clampPageLimit resolves a client's requested limit to the one the daemon
// will serve. 0 means the default; anything above the ceiling is clamped to
// it.
func clampPageLimit(requested uint32) uint32 {
	if requested == 0 {
		return pageDefaultLimit
	}
	if requested > pageMaxLimit {
		return pageMaxLimit
	}
	return requested
}

// pageSegment is one translated event's contribution to a page: the seq it
// came from and the top-level items it curated to.
//
// The seq is retained per segment because a page's boundaries fall on EVENT
// boundaries, never inside one. An event that curates to three items
// contributes all three or none: splitting it would mint a cursor that points
// into the middle of an event, and the next page would have to re-derive which
// half it already served — a correlation neither end can perform, since item
// identity is a uuid and not an offset.
type pageSegment struct {
	seq   uint64
	items []*frontendv1.ConversationItem
}

// ConversationPage serves ONE page of a workspace's conversation history.
//
// The identity ladder is the resync's, applied for the same reason: a page is
// a replay, and a replay against a generation the client never saw is a page
// of somebody else's conversation. A page whose fence does not name the live
// generation is REFUSED before any store or shim read begins, so a refusal
// costs nothing and can never half-serve.
//
// The returned page is complete or the error is non-nil. There is no partial
// page and no empty-page-on-failure: a client cannot tell an empty
// conversation from a failed read, and that ambiguity is the blank-feed bug
// this protocol's whole history has been spent closing.
func (m *Manager) ConversationPage(ctx context.Context, workspace, expectedSessionID, expectedGenerationID string, anchor PageAnchor) (*frontendv1.ConversationPage, error) {
	limit := clampPageLimit(anchor.Limit)
	m.mu.Lock()
	admission, release, err := m.admitHistoryRequest("conversation page", fmt.Sprintf("anchor=%s limit=%d", pageAnchorName(anchor), limit), workspace, expectedSessionID, expectedGenerationID)
	// The DURABLE route keeps the manager lock through the read, and the live
	// route has already released it; deferring the ladder's own release is what
	// makes that difference impossible to get wrong here (historyadmission.go).
	defer release()
	if err != nil {
		return nil, err
	}
	if admission.route == historyRouteLiveController {
		return m.pageFromController(ctx, admission.controller, anchor, limit)
	}
	return m.pageFromDurableHistory(ctx, workspace, admission.generationID, anchor, limit)
}

// pageFromController serves a page for a workspace with a live session
// controller, reading the range THROUGH THE SHIM.
func (m *Manager) pageFromController(ctx context.Context, d *sessionController, anchor PageAnchor, limit uint32) (*frontendv1.ConversationPage, error) {
	read := func(ctx context.Context, fromSeq, toSeq uint64, maxEvents uint32, onEvent func(*corev1.Event)) (pageRangeResult, error) {
		res, err := d.client.Replay(ctx, fromSeq, toSeq, maxEvents, onEvent)
		return pageRangeResult{Delivered: res.Delivered, Truncated: res.Truncated, Reason: res.Reason}, err
	}
	return m.assemblePage(ctx, d.workspace, d.sessionID, d.generationID, m.lastSeenSeq(d), anchor, limit, "shim", read)
}

// pageFromDurableHistory serves a page for a workspace with NO live session
// controller, straight from the store.
func (m *Manager) pageFromDurableHistory(ctx context.Context, workspace, generationID string, anchor PageAnchor, limit uint32) (*frontendv1.ConversationPage, error) {
	if m.cfg.DurableHistory == nil {
		return nil, fmt.Errorf("session-controller: conversation page for unwired ws %q cannot be served: no durable history source is wired", workspace)
	}
	sessionID, ok := m.cfg.Locator.Locate(workspace)
	if !ok {
		return nil, fmt.Errorf("session-controller: conversation page for unwired ws %q cannot be served: %w", workspace, errclass.ErrNoLiveSessionController)
	}
	read := func(ctx context.Context, fromSeq, toSeq uint64, maxEvents uint32, onEvent func(*corev1.Event)) (pageRangeResult, error) {
		res, err := m.cfg.DurableHistory.ReplayHistory(ctx, workspace, sessionID, fromSeq, toSeq, maxEvents, onEvent)
		return pageRangeResult{Delivered: res.Delivered, Truncated: res.Truncated, Reason: res.Reason}, err
	}
	return m.assemblePage(ctx, workspace, sessionID, generationID, m.cfg.SeqStore.LastSeq(sessionID), anchor, limit, "shim-store", read)
}

// assemblePage is the windowed backwards walk both routes share.
//
// lastSeen is the daemon's high-water mark for this conversation, and it is a
// HINT rather than an authority. See the tail anchor below for why that
// distinction is load-bearing.
func (m *Manager) assemblePage(ctx context.Context, workspace, sessionID, generationID string, lastSeen uint64, anchor PageAnchor, limit uint32, source string, read pageRangeReader) (*frontendv1.ConversationPage, error) {
	logf := dlog.Tag(dlog.Logf(m.logf), "ws", workspace, "session", sessionID, "source", source)
	// The EXCLUSIVE upper bound this page walks back from. ZERO MEANS
	// UNBOUNDED, which is what a tail anchor always is.
	//
	// A TAIL PAGE IS NEVER CAPPED AT last_seen_seq, and this is the correction
	// an end-to-end run forced. last_seen_seq is the mark a daemon wrote as it
	// CONSUMED the conversation, so it says nothing about what the store holds:
	// a daemon that never consumed this session — a fresh state database, a
	// workspace it was never wired to — carries zero, and capping the read
	// there served an EMPTY page over a store holding the entire conversation.
	// That is the blank-feed bug this protocol's whole history has been spent
	// closing, re-introduced as a bound.
	//
	// So the mark is used for the one thing it is honest about: WHERE TO START
	// READING FROM. The upper bound stays open, so whatever the store actually
	// holds above the mark is served too, and a mark that is stale or absent
	// costs a wider read rather than a wrong answer.
	var upper uint64
	if anchor.Tail {
		upper = 0
	} else {
		cursor, err := decodePageCursor(anchor.Cursor, sessionID)
		if err != nil {
			logf("session-controller: conversation page REFUSED ws=%q session=%s decision=unreadable_cursor: %v", workspace, sessionID, err)
			return nil, err
		}
		upper = cursor.beforeSeq
	}
	// The resync's floor, applied identically: the newest clear or compaction,
	// INCLUSIVE of that event itself. A client mark of 0 is passed because a
	// page has no client mark — it is asking about history, not catching up —
	// so the floor is whatever the conversation's own newest cut is.
	floor := m.replayFloorAt(workspace, sessionID, lastSeen, 0)
	// A BEFORE anchor at or below the floor has nothing left to serve:
	// everything it could page to has been cut away by a clear or a
	// compaction, which is the conversation's beginning as far as any frontend
	// is concerned. A TAIL anchor is never in this position — its bound is
	// open, and the floor is where its walk STARTS rather than where it ends.
	if !anchor.Tail && upper <= floor {
		logf("session-controller: conversation page reaches the START ws=%q session=%s upper=%d floor=%d decision=anchor_at_or_below_floor",
			workspace, sessionID, upper, floor)
		return m.newPage(workspace, sessionID, generationID, nil, pageContinuation{reachedStart: true}, 0), nil
	}

	// WHERE THE BACKWARDS WALK STARTS ITS FIRST WINDOW.
	//
	// A before page walks back from its cursor. A tail page walks back from the
	// high-water hint, which is the whole use this hint is put to: it is a
	// guess at where the interesting end of the conversation is, and a wrong
	// guess costs a wider read, never a wrong page. A hint at or below the
	// floor gives no starting point at all, so the walk begins at the floor —
	// one full pass, bounded in MEMORY by the rolling buffer below, and logged
	// so it is never mistaken for the ordinary case.
	walkFrom := upper
	if anchor.Tail {
		walkFrom = lastSeen + 1
		if walkFrom <= floor {
			logf("session-controller: conversation page has NO high-water hint ws=%q session=%s last_seen_seq=%d floor=%d decision=scan_from_floor — this daemon never consumed this conversation, so the tail is found by one pass from the floor rather than by trusting a mark that says nothing about what the store holds",
				workspace, sessionID, lastSeen, floor)
		}
	}

	var (
		segments     []pageSegment
		droppedOlder bool
		window       = uint64(pageInitialWindow)
		scanned      uint64
		lowerRead    uint64
		atFloor      bool
	)
	for {
		lower := floor
		if walkFrom > window+floor {
			lower = walkFrom - window
		} else {
			atFloor = true
		}
		var err error
		segments, droppedOlder, err = m.translateRange(ctx, workspace, sessionID, generationID, lower, upper, limit, read)
		if err != nil {
			logf("session-controller: conversation page FAILED ws=%q session=%s lower=%d upper=%d: %v", workspace, sessionID, lower, upper, err)
			return nil, err
		}
		lowerRead = lower
		scanned = walkFrom - lower
		if atFloor || countItems(segments) >= int(limit) {
			break
		}
		if scanned >= pageMaxEvents {
			// The bound stated in pageMaxEvents' doc: serve short rather than
			// widen without limit, and SAY SO, because a silently short page
			// reads as a conversation that thins out.
			logf("session-controller: conversation page SHORT ws=%q session=%s lower=%d upper=%d scanned=%d limit=%d items=%d decision=event_cap_reached — the page is served with a continuation cursor rather than widening the scan further",
				workspace, sessionID, lower, upper, scanned, limit, countItems(segments))
			break
		}
		window *= pageWindowGrowth
	}

	selected, oldestSeq, olderRemain := selectNewest(segments, int(limit))
	// CONTINUATION. There is more history above this page's oldest item
	// whenever the walk left item-bearing segments behind — either still in the
	// buffer (olderRemain) or dropped out of its oldest end (droppedOlder) — or
	// whenever it stopped before reaching the floor. Reaching the floor with
	// nothing left behind is the one case that is genuinely the beginning.
	reachedStart := atFloor && !olderRemain && !droppedOlder
	var cursor string
	if !reachedStart {
		before := oldestSeq
		if before == 0 {
			// Nothing was selected at all, so the next page continues from the
			// lowest seq this walk actually read rather than from an item
			// boundary that does not exist.
			before = lowerRead
		}
		cursor = encodePageCursor(pageCursor{sessionID: sessionID, beforeSeq: before})
	}
	// live_join_seq is TAIL ONLY, and it is the newest seq this page is
	// current through: the client stores it as its from_seq, and because a
	// resync replays from that seq INCLUSIVE, every event the session produced
	// after the page was minted is above it and is replayed. The splice is
	// gap-free by construction rather than by timing.
	var liveJoinSeq uint64
	if anchor.Tail && len(selected) > 0 {
		liveJoinSeq = selected[len(selected)-1].seq
	}
	items := flattenItems(selected)
	logf("session-controller: conversation page SERVED ws=%q session=%s anchor=%s limit=%d items=%d segments=%d scanned=%d floor=%d upper=%d continuation=%s live_join_seq=%d",
		workspace, sessionID, pageAnchorName(anchor), limit, len(items), len(selected), scanned, floor, upper, continuationName(reachedStart), liveJoinSeq)
	return m.newPage(workspace, sessionID, generationID, items, pageContinuation{reachedStart: reachedStart, cursor: cursor}, liveJoinSeq), nil
}

// translateRange reads one seq range and returns what it curated to, oldest
// first.
//
// The consumer is built fresh per range on purpose. It carries fold state (the
// skill correlator, the async windows, the accounting reducer) that is only
// meaningful for the events it has actually seen, and re-using one across a
// widening window would have the second read fold against a first read's
// leftovers.
func (m *Manager) translateRange(ctx context.Context, workspace, sessionID, generationID string, lower, upper uint64, limit uint32, read pageRangeReader) ([]pageSegment, bool, error) {
	capture := &pageCapture{}
	cons := m.historyConsumer(workspace, sessionID, capture)
	// The generation the page's items are fenced under. The consumer stamps
	// every delta it curates with ssm.Fence(sessionID, generationID), and
	// newPage stamps the page itself with the same pair, so an item's fence and
	// its page's fence can never disagree.
	//
	// The durable RECEIPT ledger is deliberately NOT bound (see
	// durableConsumer): a page is a read, and retiring a receipt row because
	// somebody scrolled up would be a write nobody asked for.
	cons.generationID = generationID
	if err := m.hydratePersistedAccounting(cons, sessionID); err != nil {
		return nil, false, err
	}
	// THE ROLLING BUFFER, and the reason a page is bounded in MEMORY however
	// wide its range gets.
	//
	// A page serves the NEWEST `limit` items of the range, so only the newest
	// item-bearing segments can ever be selected. Keeping `limit+1` of them is
	// exactly enough: `limit` segments carry at least `limit` items (every
	// segment holds at least one), and the extra one is what proves there is
	// something older to continue from. Everything that falls out of the oldest
	// end is reported as `droppedOlder` rather than forgotten, because that is
	// the difference between "this page reaches the beginning" and "this page
	// is where the daemon stopped keeping".
	//
	// It is what makes the no-high-water-hint full pass above affordable: a
	// 259,000-event conversation is translated in one stream and only eleven
	// segments are ever resident.
	keep := int(limit) + 1
	var (
		segments []pageSegment
		dropped  bool
	)
	res, err := read(ctx, exclusiveLowerBound(lower), upper, pageMaxEvents, func(ev *corev1.Event) {
		before := len(capture.deltas)
		cons.pushConversation(ev, false)
		for _, cd := range capture.deltas[before:] {
			if len(cd.GetItems()) == 0 {
				continue
			}
			segments = append(segments, pageSegment{seq: ev.GetSeq(), items: cd.GetItems()})
			if len(segments) > keep {
				segments = segments[1:]
				dropped = true
			}
		}
		// The capture's own backlog is released with the segments it produced,
		// so a full pass does not retain every delta it ever translated.
		capture.deltas = capture.deltas[:0]
	})
	if err != nil {
		return nil, false, fmt.Errorf("session-controller: conversation page range read for ws %q (lower=%d upper=%d) failed after %d event(s): %w",
			workspace, lower, upper, res.Delivered, err)
	}
	if res.Truncated {
		// A truncated range is NOT served as a short page: a page's own
		// shortness is a fact about the conversation, and presenting a
		// truncated read as one would state that fact falsely.
		return nil, false, fmt.Errorf("%w: conversation page for ws %q (lower=%d upper=%d) read %d event(s): %s",
			ErrRepullTruncated, workspace, lower, upper, res.Delivered, res.Reason)
	}
	return segments, dropped, nil
}

// selectNewest takes whole segments from the newest end until the limit is
// reached, and reports whether any item-bearing segment was left behind.
//
// WHOLE SEGMENTS, always. A segment is one event's items, and a page boundary
// inside an event would mint a cursor pointing into the middle of it — see
// pageSegment. One consequence is deliberate and worth stating: a single event
// whose items exceed the limit is served WHOLE rather than served empty, so a
// page is never nothing merely because the first thing in it was large.
func selectNewest(segments []pageSegment, limit int) (selected []pageSegment, oldestSeq uint64, olderRemain bool) {
	count := 0
	first := len(segments)
	for i := len(segments) - 1; i >= 0; i-- {
		n := len(segments[i].items)
		if count > 0 && count+n > limit {
			break
		}
		count += n
		first = i
		if count >= limit {
			break
		}
	}
	if first >= len(segments) {
		return nil, 0, len(segments) > 0
	}
	selected = segments[first:]
	return selected, segments[first].seq, first > 0
}

// countItems totals the top-level items across segments.
func countItems(segments []pageSegment) int {
	n := 0
	for _, s := range segments {
		n += len(s.items)
	}
	return n
}

// flattenItems concatenates the selected segments' items, oldest first.
func flattenItems(selected []pageSegment) []*frontendv1.ConversationItem {
	var items []*frontendv1.ConversationItem
	for _, s := range selected {
		items = append(items, s.items...)
	}
	return items
}

// newPage builds the wire message, stamping the fence at MINT time.
//
// The fence is read here rather than by the caller because "the fence as of
// the moment this page was assembled" is the only fence the contract permits:
// a client byte-compares it against the workspace's current state and discards
// the page whole when they differ, and a fence read before the assembly would
// claim currency the page does not have.
func (m *Manager) newPage(workspace, sessionID, generationID string, items []*frontendv1.ConversationItem, continuation pageContinuation, liveJoinSeq uint64) *frontendv1.ConversationPage {
	page := &frontendv1.ConversationPage{
		Workspace:   workspace,
		Items:       items,
		LiveJoinSeq: liveJoinSeq,
		Fence:       ssm.Fence(sessionID, generationID),
	}
	if continuation.reachedStart {
		page.Continuation = &frontendv1.ConversationPage_Start{Start: &frontendv1.ConversationPageStart{}}
		return page
	}
	page.Continuation = &frontendv1.ConversationPage_More{More: &frontendv1.ConversationPageMore{Cursor: continuation.cursor}}
	return page
}

// pageContinuation is the resolved answer to "is there more above this page",
// passed as ONE value so the two halves cannot be set inconsistently: a start
// arm never carries a cursor, and a more arm always does.
type pageContinuation struct {
	reachedStart bool
	cursor       string
}

// pageAnchorName names the anchor for the log line.
func pageAnchorName(a PageAnchor) string {
	if a.Tail {
		return "tail"
	}
	return "before"
}

// continuationName names the continuation arm for the log line.
func continuationName(reachedStart bool) string {
	if reachedStart {
		return "start"
	}
	return "more"
}

// compile-time proof the live route's replay really has the shape the windowed
// walk was written against.
var _ interface {
	Replay(context.Context, uint64, uint64, uint32, func(*corev1.Event)) (shimclient.ReplayResult, error)
} = (*shimclient.Client)(nil)
