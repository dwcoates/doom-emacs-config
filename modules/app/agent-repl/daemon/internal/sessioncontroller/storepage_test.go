package sessioncontroller

import (
	"context"
	"errors"
	"fmt"
	"sync"
	"testing"

	corev1 "agentrepl/proto/agentshim/core/v1"

	"claude-repld/internal/storehistory"
)

// THE HISTORY PAGE'S RECORDS NOW COME FROM THE STORE'S BOUNDED PAGE.
//
// Every case here is one property of that seam: the anchor the daemon issues,
// the position it copies rather than computes, the reversal it owes the
// frontend, the retention fact it refuses to promote into a beginning, and the
// failure it refuses to serve as an empty page.

// messagePageSpy is a MessagePageSource over a canned event list, shaped like
// the store: one message per record, ten messages a page, NEWEST FIRST, and a
// last_page_seq the SPY mints so a test asserting the daemon copies it is
// asserting something.
type messagePageSpy struct {
	mu sync.Mutex
	// anchors records one entry per page request, in order.
	anchors []storehistory.PageAnchor
	// served records the pages handed back, so a test can compare the anchor
	// of one request against the value the previous page minted.
	served []*corev1.MessagePage
	events []*corev1.Event
	// override, when set, is returned instead of a page built from events —
	// the way a test states a boundary arm the fixture would not produce.
	override *corev1.MessagePage
	err      error
}

func (s *messagePageSpy) MessagePage(_ context.Context, _, _ string, anchor storehistory.PageAnchor) (*corev1.MessagePage, error) {
	s.mu.Lock()
	defer s.mu.Unlock()
	s.anchors = append(s.anchors, anchor)
	if s.err != nil {
		return nil, s.err
	}
	if s.override != nil {
		s.served = append(s.served, s.override)
		return s.override, nil
	}
	// Everything strictly below the anchor, exactly as the store's
	// `seq < :anchor` selection reads it. A head anchor names nothing, so the
	// whole conversation is below it.
	var below []*corev1.Event
	for _, ev := range s.events {
		if anchor.Head || ev.GetSeq() < anchor.BeforeSeq {
			below = append(below, ev)
		}
	}
	take := below
	if len(take) > 10 {
		take = take[len(take)-10:]
	}
	page := &corev1.MessagePage{}
	// NEWEST FIRST into the slots, which is what the storage contract states.
	slots := []func(*corev1.StoredMessage){
		func(m *corev1.StoredMessage) { page.Message_1 = m },
		func(m *corev1.StoredMessage) { page.Message_2 = m },
		func(m *corev1.StoredMessage) { page.Message_3 = m },
		func(m *corev1.StoredMessage) { page.Message_4 = m },
		func(m *corev1.StoredMessage) { page.Message_5 = m },
		func(m *corev1.StoredMessage) { page.Message_6 = m },
		func(m *corev1.StoredMessage) { page.Message_7 = m },
		func(m *corev1.StoredMessage) { page.Message_8 = m },
		func(m *corev1.StoredMessage) { page.Message_9 = m },
		func(m *corev1.StoredMessage) { page.Message_10 = m },
	}
	for i := 0; i < len(take); i++ {
		ev := take[len(take)-1-i]
		slots[i](&corev1.StoredMessage{MessageId: fmt.Sprintf("m%d", ev.GetSeq()), Records: []*corev1.Event{ev}})
	}
	if len(take) > 0 {
		page.LastPageSeq = take[0].GetSeq()
	}
	if len(below) > len(take) {
		page.Boundary = &corev1.MessagePage_More{More: &corev1.HistoryRemainsBelow{}}
	} else {
		page.Boundary = &corev1.MessagePage_Floor{Floor: &corev1.HistoryAtRetainedFloor{}}
	}
	s.served = append(s.served, page)
	return page, nil
}

func (s *messagePageSpy) requests() []storehistory.PageAnchor {
	s.mu.Lock()
	defer s.mu.Unlock()
	return append([]storehistory.PageAnchor(nil), s.anchors...)
}

func (s *messagePageSpy) pages() []*corev1.MessagePage {
	s.mu.Lock()
	defer s.mu.Unlock()
	return append([]*corev1.MessagePage(nil), s.served...)
}

func TestAFirstHistoryPageAnchorsAtTheStoresHead(t *testing.T) {
	// Arrange — the head is a fact the STORE resolves. A daemon that named a
	// seq for it would be authoring exactly the position this contract removes.
	h := newHistoryHarness(t, pageTextEvents(t, 30))

	// Act.
	h.firstPage(t, "r1")

	// Assert.
	got := h.pages.requests()
	if len(got) != 1 || !got[0].Head {
		t.Fatalf("first page issued anchors %+v, want exactly one HEAD anchor", got)
	}
}

func TestANextHistoryPageCopiesTheStoresLastPageSeqVerbatim(t *testing.T) {
	// Arrange — a continuation names a place the caller has DEMONSTRABLY BEEN,
	// and the only such value is the one the store minted. Arithmetic on it
	// here would be a position of the daemon's own.
	h := newHistoryHarness(t, pageTextEvents(t, 30))
	h.firstPage(t, "r1")
	minted := h.pages.pages()[0].GetLastPageSeq()

	// Act.
	h.nextPage(t, "r1")

	// Assert.
	got := h.pages.requests()
	if len(got) != 2 {
		t.Fatalf("page requests = %d, want 2", len(got))
	}
	if got[1].Head {
		t.Fatal("the next page anchored at the head instead of continuing below the first")
	}
	if got[1].BeforeSeq != minted {
		t.Fatalf("next page before_seq = %d, want the store's own last_page_seq %d carried verbatim", got[1].BeforeSeq, minted)
	}
}

func TestAStorePageIsReversedIntoAnOldestFirstHistoryPage(t *testing.T) {
	// Arrange — the store page is NEWEST FIRST because a backward-anchored read
	// can be nothing else; the frontend page is OLDEST FIRST so a paged message
	// renders with the code that renders a pushed one. The reversal is the
	// daemon's.
	h := newHistoryHarness(t, pageTextEvents(t, 12))

	// Act.
	page := h.firstPage(t, "r1")

	// Assert.
	got := historyPageUUIDs(page)
	if len(got) != 10 || got[0] != "u3" || got[9] != "u12" {
		t.Fatalf("history page = %v, want u3..u12 oldest first", got)
	}
}

func TestTheRetainedFloorArmIsNotHistoryAtStart(t *testing.T) {
	// Arrange — the store says it reached the oldest RETAINED record. That is
	// the store's own retention fact and says nothing about where the
	// conversation began; the daemon's floor here is untouched, so nothing
	// establishes a beginning.
	h := newHistoryHarness(t, pageTextEvents(t, 30))
	h.pages.override = &corev1.MessagePage{
		Message_1:   &corev1.StoredMessage{MessageId: "m20", Records: []*corev1.Event{pageAssistantEvent(t, 20, "u20", nil)}},
		LastPageSeq: 20,
		Boundary:    &corev1.MessagePage_Floor{Floor: &corev1.HistoryAtRetainedFloor{}},
	}

	// Act.
	page := h.firstPage(t, "r1")

	// Assert — the client keeps its load-more affordance.
	if page.GetStart() != nil {
		t.Fatal("the store's retained-floor arm was promoted into HistoryAtStart, retiring load-more on a retention policy")
	}
	if page.GetMore() == nil {
		t.Fatal("a page below the daemon's floor carried neither continuation arm")
	}
}

func TestTheDaemonsOwnFloorIsWhatMintsHistoryAtStart(t *testing.T) {
	// Arrange — the page reaches the oldest seq the conversation could still
	// serve. THIS is knowledge the daemon holds separately from the store's
	// retention, and it is the only thing that retires load-more.
	h := newHistoryHarness(t, pageTextEvents(t, 8))

	// Act.
	page := h.firstPage(t, "r1")

	// Assert.
	if page.GetStart() == nil {
		t.Fatalf("a page covering the whole conversation (%v) did not reach HistoryAtStart", historyPageUUIDs(page))
	}
}

func TestAStorePageReadFailureIsRefusedRatherThanServedEmpty(t *testing.T) {
	// Arrange — an empty page is a claim about the CONVERSATION and a failed
	// read is a claim about the STORE. A client cannot tell them apart, which
	// is the blank-feed bug.
	h := newHistoryHarness(t, pageTextEvents(t, 30))
	h.pages.err = errors.New("the store is unreachable")

	// Act.
	page, err := h.m.FirstConversationHistoryPage(context.Background(), "r1", "ws")

	// Assert.
	if err == nil {
		t.Fatalf("a failed store page was served as a page with %d slot(s)", len(historyPageUUIDs(page)))
	}
	if page != nil {
		t.Fatalf("a failed store page still produced a page: %v", historyPageUUIDs(page))
	}
}

func TestAStorePageWithNoBoundaryArmIsRefused(t *testing.T) {
	// Arrange — the boundary oneof is the store's whole statement about older
	// history. An unset arm is a protocol violation, not a third answer, and
	// defaulting it would invent a claim the store never made.
	h := newHistoryHarness(t, pageTextEvents(t, 30))
	h.pages.override = &corev1.MessagePage{
		Message_1:   &corev1.StoredMessage{MessageId: "m20", Records: []*corev1.Event{pageAssistantEvent(t, 20, "u20", nil)}},
		LastPageSeq: 20,
	}

	// Act.
	_, err := h.m.FirstConversationHistoryPage(context.Background(), "r1", "ws")

	// Assert.
	if err == nil {
		t.Fatal("a page with no boundary arm was served, stating a continuation the store never claimed")
	}
}

func TestAHistoryPageWithNoBoundedPageSourceIsRefused(t *testing.T) {
	// Arrange — with no page source there is no bounded read, and falling back
	// to the forward scan would quietly restore the unbounded cold open this
	// whole path exists to end.
	h := newHistoryHarness(t, pageTextEvents(t, 30))
	h.m.cfg.MessagePages = nil

	// Act.
	_, err := h.m.FirstConversationHistoryPage(context.Background(), "r1", "ws")

	// Assert.
	if err == nil {
		t.Fatal("a history page was served with no bounded message page source wired")
	}
}
