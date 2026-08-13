package sessioncontroller

import (
	"context"
	"fmt"

	frontendv1 "agentrepl/proto/frontend/v1"
	protocolv1 "agentrepl/proto/protocol/v1"

	"claude-repld/internal/dlog"
	"claude-repld/internal/errclass"
	"claude-repld/internal/storehistory"
)

// THE PAGE READ IS NOW BOUNDED AT THE STORE, not merely at the daemon's
// forwarding.
//
// # What this replaces, and why relocating the cost was not enough
//
// The windowed backwards walk (conversationpage.go) pages correctly and is
// still the only reader either route had: neither `Subscribe` nor
// `ReplayRequest` can express "the newest ten messages", because both read
// FORWARD FROM A LOWER BOUND, so a page had to guess a from_seq low enough to
// cover its tail and widen until it did. The frontend then received ten
// messages — over a store read that had streamed the volume anyway. The cost
// moved; it did not go away.
//
// `MessagePageRequest` is the verb that was missing. `MessagePageHead` asks for
// the newest page WITHOUT naming a seq, `before_seq` continues below a page
// already received, and the store resolves message OWNERSHIP itself so the ten
// it returns are ten renderable rows rather than ten records.
//
// # IT IS THE SAME CURATOR, and that is the whole reason this file is shaped
// # the way it is
//
// The obvious way to consume a MessagePage is to translate StoredMessages into
// feed messages here. That would be a SECOND CURATOR: the withhold passes, the
// clear/compact coalescing, the provenance stamp read from the merge lease's
// durable ledger, the keep-alive exclusion and the spawned-work stamps all
// live in consumer.pushConversation, and a page-shaped translator beside it is
// two implementations that must be kept in agreement forever — whose
// disagreement shows up as a message that renders one way live and another way
// paged.
//
// So nothing here translates anything. A StoredMessage is UNPACKED INTO THE
// RECORDS IT ALREADY CARRIES and those records are driven through
// consumer.pushConversation exactly as the durable replay and the shim re-pull
// drive them, into the same pageCapture sink the windowed walk uses. What
// changed is WHERE THE RECORDS CAME FROM. What did not change is who turns a
// record into a feed message, and there is still exactly one answer to that.
//
// # The reversal is the daemon's
//
// The store page is NEWEST FIRST (a backward-anchored read can be nothing
// else) and the frontend page is OLDEST FIRST, so a frontend renders a paged
// message with the code that renders a pushed one. The reversal happens here,
// before the curator sees anything, so the curator folds records in the same
// ascending order every other history route hands it.
//
// # HistoryAtRetainedFloor is NOT HistoryAtStart
//
// The store's `floor` arm is the store's OWN RETENTION FACT: it says the page
// reached the oldest record still retained, and says nothing about whether the
// conversation began there. Collapsing it into HistoryAtStart would retire a
// client's load-more affordance on the strength of a retention policy.
//
// The daemon does have its own, separate knowledge of a beginning, and it is
// the replay floor every other route already uses: the newest clear or
// compaction, below which history is what a frontend would discard anyway.
// A page whose oldest covered seq sits at or below that floor reaches the
// beginning OF THE LIVE CONVERSATION, and that — not the store's arm — is what
// mints HistoryAtStart.

// MessagePageSource fetches ONE bounded, backward-anchored page of messages.
// Satisfied by *storehistory.Reader.
//
// The anchor is the wire's, unchanged: a head arm that names nothing, or a
// before_seq copied VERBATIM from a page the store minted. There is no arm by
// which a caller states a position of its own.
type MessagePageSource interface {
	MessagePage(ctx context.Context, workspace, sessionID string, anchor storehistory.PageAnchor) (*protocolv1.MessagePage, error)
}

// messagePageFetch reads ONE bounded page for an anchor. It is the only thing
// the two routes differ by: the unwired one dials the store, the live one asks
// the shim, and everything downstream — the anchor, the curation, the boundary
// ruling and the position copied verbatim — is the code below, once.
type messagePageFetch func(ctx context.Context, anchor storehistory.PageAnchor) (*protocolv1.MessagePage, error)

// pageFromStorePage serves one history page from the store's bounded page read.
func (m *Manager) pageFromStorePage(ctx context.Context, workspace, generationID string, resolve pageBoundResolver, first bool) (pageOutcome, error) {
	if m.cfg.MessagePages == nil {
		return pageOutcome{}, fmt.Errorf("session-controller: conversation history page for unwired ws %q cannot be served: no bounded message page source is wired", workspace)
	}
	sessionID, ok := m.cfg.Locator.Locate(workspace)
	if !ok {
		return pageOutcome{}, fmt.Errorf("session-controller: conversation history page for unwired ws %q cannot be served: %w", workspace, errclass.ErrNoLiveSessionController)
	}
	fetch := func(ctx context.Context, anchor storehistory.PageAnchor) (*protocolv1.MessagePage, error) {
		return m.cfg.MessagePages.MessagePage(ctx, workspace, sessionID, anchor)
	}
	return m.pageFromMessagePage(ctx, workspace, sessionID, generationID, "store-page", m.cfg.SeqStore.LastSeq(sessionID), fetch, resolve, first)
}

// pageFromMessagePage is the bounded-page body BOTH routes share.
//
// lastSeen is the daemon's high-water mark for this conversation, used for one
// thing only: resolving the replay floor that decides whether this page reached
// the conversation's BEGINNING. It never bounds the read — the anchor does that,
// and the anchor is the wire's.
func (m *Manager) pageFromMessagePage(ctx context.Context, workspace, sessionID, generationID, source string, lastSeen uint64, fetch messagePageFetch, resolve pageBoundResolver, first bool) (pageOutcome, error) {
	logf := dlog.Tag(dlog.Logf(m.logf), "ws", workspace, "session", sessionID, "source", source)
	bound, err := resolve(sessionID)
	if err != nil {
		logf("session-controller: history page REFUSED ws=%q session=%s decision=unresolvable_bound: %v", workspace, sessionID, err)
		return pageOutcome{}, err
	}
	// THE ANCHOR, AND THE ONE PLACE A POSITION COULD HAVE BEEN INVENTED. A
	// first page anchors at the HEAD, which names nothing at all. A next page
	// carries the daemon's own record of where this reader is — which is a
	// value the STORE minted as last_page_seq and this daemon stored verbatim.
	// Nothing is added to it, subtracted from it, or derived from the records
	// on a page.
	anchor := storehistory.PageAnchor{Head: bound.tail}
	if !bound.tail {
		anchor.BeforeSeq = bound.upper
	}
	page, err := fetch(ctx, anchor)
	if err != nil {
		// LOUD. An empty page is a claim about the conversation; a failed read
		// is a claim about the store, and a client cannot tell them apart.
		logf("session-controller: history page FAILED ws=%q session=%s anchor=%s before_seq=%d: %v", workspace, sessionID, bound.name, anchor.BeforeSeq, err)
		return pageOutcome{}, fmt.Errorf("session-controller: reading the bounded message page for ws %q (anchor=%s before_seq=%d) failed: %w",
			workspace, bound.name, anchor.BeforeSeq, err)
	}
	if page == nil {
		return pageOutcome{}, fmt.Errorf("session-controller: the bounded message page for ws %q (anchor=%s) came back nil, which is neither a page nor a failure", workspace, bound.name)
	}
	if _, set := storePageBoundary(page); !set {
		// The boundary oneof is the store's whole statement about older
		// history. An unset arm is a PROTOCOL VIOLATION, not a third answer, so
		// it is surfaced rather than defaulted into either arm.
		logf("session-controller: history page REFUSED ws=%q session=%s decision=boundary_unset last_page_seq=%d", workspace, sessionID, page.GetLastPageSeq())
		return pageOutcome{}, fmt.Errorf("session-controller: the bounded message page for ws %q set no boundary arm, so whether older history remains is unstated", workspace)
	}

	items, newestSeq, records, err := m.curateStorePage(workspace, sessionID, generationID, page)
	if err != nil {
		return pageOutcome{}, err
	}

	// THE DAEMON'S OWN KNOWLEDGE OF A BEGINNING, and the only thing that mints
	// one. The store's retained-floor arm is deliberately not consulted here.
	floor := m.replayFloorAt(workspace, sessionID, lastSeen, 0)
	reachedStart := page.GetLastPageSeq() <= startSeq(floor)
	// live_join_seq is FIRST PAGES ONLY: the newest seq this page is current
	// through, so the client splices onto the live stream by construction.
	var liveJoinSeq uint64
	if first {
		liveJoinSeq = newestSeq
	}
	logf("session-controller: history page SERVED from the STORE'S BOUNDED PAGE ws=%q session=%s anchor=%s messages=%d records=%d last_page_seq=%d store_boundary=%s floor=%d continuation=%s live_join_seq=%d",
		workspace, sessionID, bound.name, len(items), records, page.GetLastPageSeq(), storePageBoundaryName(page), floor, continuationName(reachedStart), liveJoinSeq)
	return pageOutcome{
		items:        items,
		reachedStart: reachedStart,
		liveJoinSeq:  liveJoinSeq,
		// COPIED VERBATIM. The next page's anchor is the store's own value, and
		// the daemon is its custodian rather than its author.
		nextBeforeSeq: page.GetLastPageSeq(),
	}, nil
}

// curateStorePage drives the page's records THROUGH consumer.pushConversation —
// the one curation chokepoint every replay route funnels through — and returns
// the feed messages it curated to, OLDEST FIRST.
//
// The reversal happens here and only here: the page's message slots are read
// newest first and walked backwards, while each message's own records are
// already oldest first and are pushed in the order they carry.
func (m *Manager) curateStorePage(workspace, sessionID, generationID string, page *protocolv1.MessagePage) ([]*frontendv1.Message, uint64, int, error) {
	capture := &pageCapture{}
	cons := m.historyConsumer(workspace, sessionID, capture)
	// The generation the curating consumer runs under. It fences the DELTAS the
	// consumer emits, and pageCapture keeps only the messages out of those, so
	// this token never reaches the wire.
	//
	// The durable RECEIPT ledger is deliberately NOT bound (see durableConsumer):
	// a page is a READ, and retiring a receipt row because somebody scrolled up
	// would be a write nobody asked for.
	cons.generationID = generationID
	if err := m.hydratePersistedAccounting(cons, sessionID); err != nil {
		return nil, 0, 0, err
	}
	stored := storePageMessages(page)
	var (
		items     []*frontendv1.Message
		newestSeq uint64
		records   int
	)
	for i := len(stored) - 1; i >= 0; i-- {
		for _, ev := range stored[i].GetRecords() {
			if ev == nil {
				return nil, 0, 0, fmt.Errorf("session-controller: the bounded message page for ws %q carries a nil record under message %q", workspace, stored[i].GetMessageId())
			}
			records++
			if ev.GetSeq() > newestSeq {
				newestSeq = ev.GetSeq()
			}
			before := len(capture.deltas)
			cons.pushConversation(ev, false)
			for _, cd := range capture.deltas[before:] {
				items = append(items, cd.GetMessages()...)
			}
			capture.deltas = capture.deltas[:0]
		}
	}
	return items, newestSeq, records, nil
}

// storePageMessages reads the ten discrete slots, NEWEST FIRST, and nothing
// else. There is no path here by which an eleventh message reaches a consumer,
// because there is no eleventh field to read.
func storePageMessages(page *protocolv1.MessagePage) []*protocolv1.StoredMessage {
	slots := []*protocolv1.StoredMessage{
		page.GetMessage_1(), page.GetMessage_2(), page.GetMessage_3(), page.GetMessage_4(), page.GetMessage_5(),
		page.GetMessage_6(), page.GetMessage_7(), page.GetMessage_8(), page.GetMessage_9(), page.GetMessage_10(),
	}
	var filled []*protocolv1.StoredMessage
	for _, slot := range slots {
		if slot != nil {
			filled = append(filled, slot)
		}
	}
	return filled
}

// storePageBoundary reports which boundary arm the store set, keeping the two
// arms distinct and reporting an UNSET oneof as unset rather than as either.
func storePageBoundary(page *protocolv1.MessagePage) (atRetainedFloor bool, set bool) {
	switch page.GetBoundary().(type) {
	case *protocolv1.MessagePage_More:
		return false, true
	case *protocolv1.MessagePage_Floor:
		return true, true
	default:
		return false, false
	}
}

// storePageBoundaryName names the boundary arm for the log line, so a record
// says what the STORE claimed alongside what the daemon concluded.
func storePageBoundaryName(page *protocolv1.MessagePage) string {
	atFloor, set := storePageBoundary(page)
	switch {
	case !set:
		return "unset"
	case atFloor:
		return "retained_floor"
	default:
		return "more"
	}
}

// startSeq converts the replay floor into the OLDEST SEQ a page could still
// serve.
//
// The floor is the INCLUSIVE first seq a replay covers, and a store's seq space
// starts at 1, so a floor of 0 means "nothing has been cut, and the oldest
// thing there could be is seq 1". A page whose oldest covered seq is at or
// below this has nothing older left to serve — every record below it is either
// non-existent or discarded by a clear or a compaction.
func startSeq(floor uint64) uint64 {
	if floor == 0 {
		return 1
	}
	return floor
}

// compile-time proof the durable reader really is the bounded page source this
// package asks for.
var _ MessagePageSource = (*storehistory.Reader)(nil)
