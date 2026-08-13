package sessioncontroller

import (
	"context"
	"errors"
	"sync"
	"testing"

	corev1 "agentrepl/proto/agentshim/core/v1"
	frontendv1 "agentrepl/proto/frontend/v1"

	"claude-repld/internal/shimclient"
	"claude-repld/internal/storehistory"
)

// THE LIVE WORKSPACE — the common case — NOW TAKES THE BOUNDED PAGE.
//
// Every case here is one property of that route: the anchor it issues, the
// position it copies rather than computes, the refusal it surfaces instead of
// serving an empty page, the retention fact it refuses to promote into a
// beginning, and the reader position which is exactly the one the unwired route
// keeps.
//
// The harness wires NO MessagePageSource at all, so a page that arrived by
// dialling the store could not have been served: everything asserted below came
// through the shim.

// pageClient is a live shim client that serves scripted bounded pages. It
// embeds fakeClient, whose own MessagePage REFUSES, so a case that forgets to
// script one fails loudly rather than reading as an empty conversation.
type pageClient struct {
	fakeClient

	mu      sync.Mutex
	spy     *messagePageSpy
	anchors []shimclient.MessagePageAnchor
	err     error
}

func (c *pageClient) MessagePage(ctx context.Context, anchor shimclient.MessagePageAnchor) (*corev1.MessagePage, error) {
	c.mu.Lock()
	c.anchors = append(c.anchors, anchor)
	err := c.err
	c.mu.Unlock()
	if err != nil {
		return nil, err
	}
	return c.spy.MessagePage(ctx, "ws", "s1", storehistory.PageAnchor{Head: anchor.Head, BeforeSeq: anchor.BeforeSeq})
}

func (c *pageClient) requests() []shimclient.MessagePageAnchor {
	c.mu.Lock()
	defer c.mu.Unlock()
	return append([]shimclient.MessagePageAnchor(nil), c.anchors...)
}

// livePageHarness is a Manager whose "ws" workspace has a LIVE session
// controller, so the positionless history surface takes the live route.
type livePageHarness struct {
	m         *Manager
	client    *pageClient
	applier   *fakeApplier
	positions *fakePositions
}

func newLivePageHarness(t *testing.T, events []*corev1.Event) *livePageHarness {
	t.Helper()
	h := &livePageHarness{
		applier: &fakeApplier{},
		client:  &pageClient{spy: &messagePageSpy{events: events}},
	}
	m, err := New(Config{
		Push:              &fakePusher{},
		SSM:               h.applier,
		Spawner:           &fakeSpawner{},
		Locator:           fakeLocator{m: map[string]string{"ws": "s1"}},
		SeqStore:          &fakeSeqStore{seq: map[string]uint64{}},
		ClearCompactStore: newFakeClearCompactStore(),
		TurnAccountings:   emptyTurnAccountingStore{},
		ProtocolVersion:   "1",
		Source:            stubSource{},
		FileDiagnostics:   fakeFileDiagnosticPersister{},
		newClient:         func(shimclient.Config) sessionClient { return h.client },
	})
	if err != nil {
		t.Fatalf("New: %v", err)
	}
	t.Cleanup(m.Close)
	if err := m.Ensure("ws"); err != nil {
		t.Fatalf("Ensure: %v", err)
	}
	h.m = m
	h.positions = h.applier.positions()
	return h
}

func (h *livePageHarness) firstPage(t *testing.T, reader string) *frontendv1.ConversationHistoryPage {
	t.Helper()
	page, err := h.m.FirstConversationHistoryPage(context.Background(), reader, "ws")
	if err != nil {
		t.Fatalf("FirstConversationHistoryPage(reader=%q): %v", reader, err)
	}
	return page
}

func (h *livePageHarness) nextPage(t *testing.T, reader string) *frontendv1.ConversationHistoryPage {
	t.Helper()
	page, err := h.m.NextConversationHistoryPage(context.Background(), reader, "ws")
	if err != nil {
		t.Fatalf("NextConversationHistoryPage(reader=%q): %v", reader, err)
	}
	return page
}

// generationID is the live controller's generation, which is what a reader
// position is filed under.
func (h *livePageHarness) generationID(t *testing.T) string {
	t.Helper()
	d, err := h.m.existing("ws")
	if err != nil {
		t.Fatalf("existing: %v", err)
	}
	return d.generationID
}

func TestALiveWorkspacesFirstPageAnchorsAtTheHeadThroughTheShim(t *testing.T) {
	// Arrange — a live workspace is the COMMON case, and it was the one still
	// taking the windowed backwards walk.
	h := newLivePageHarness(t, pageTextEvents(t, 30))

	// Act.
	h.firstPage(t, "r1")

	// Assert.
	got := h.client.requests()
	if len(got) != 1 || !got[0].Head {
		t.Fatalf("first page issued shim anchors %+v, want exactly one HEAD anchor", got)
	}
}

func TestALiveWorkspacesNextPageCopiesTheStoresLastPageSeqVerbatim(t *testing.T) {
	// Arrange — the continuation is a value the STORE minted; arithmetic on it
	// here would be a position of the daemon's own.
	h := newLivePageHarness(t, pageTextEvents(t, 30))
	h.firstPage(t, "r1")
	minted := h.client.spy.pages()[0].GetLastPageSeq()

	// Act.
	h.nextPage(t, "r1")

	// Assert.
	got := h.client.requests()
	if len(got) != 2 {
		t.Fatalf("shim page requests = %d, want 2", len(got))
	}
	if got[1].Head || got[1].BeforeSeq != minted {
		t.Fatalf("next page anchor = %+v, want before_seq=%d carried verbatim", got[1], minted)
	}
}

func TestALiveWorkspacesRefusedPageIsAFailureAndNotAnEmptyPage(t *testing.T) {
	// Arrange — the shim reports failure as a Nack bearing the page's request
	// id. Read as an empty page it would be indistinguishable from a
	// conversation with no history.
	h := newLivePageHarness(t, pageTextEvents(t, 30))
	h.client.err = shimclient.ErrMessagePageRefused

	// Act.
	page, err := h.m.FirstConversationHistoryPage(context.Background(), "r1", "ws")

	// Assert.
	if !errors.Is(err, shimclient.ErrMessagePageRefused) {
		t.Fatalf("FirstConversationHistoryPage error = %v, want the shim's refusal", err)
	}
	if page != nil {
		t.Fatalf("a refused read returned a page %+v, and a refusal must never arrive as history", page)
	}
}

func TestALiveWorkspacesRetainedFloorIsNotAStart(t *testing.T) {
	// Arrange — retention is the STORE'S fact; the conversation's beginning is
	// the daemon's. This page reaches the oldest RETAINED record while sitting
	// far above the daemon's own replay floor.
	h := newLivePageHarness(t, pageTextEvents(t, 30))
	h.client.spy.override = &corev1.MessagePage{
		LastPageSeq: 50,
		Boundary:    &corev1.MessagePage_Floor{Floor: &corev1.HistoryAtRetainedFloor{}},
	}

	// Act.
	page := h.firstPage(t, "r1")

	// Assert.
	if page.GetStart() != nil {
		t.Fatal("the store's retained-floor arm was promoted into HistoryAtStart, retiring the client's load-more affordance on the strength of a retention policy")
	}
	if page.GetMore() == nil {
		t.Fatalf("continuation = %v, want HistoryHasMore", page.GetContinuation())
	}
}

func TestALiveWorkspacesPageWithAnUnsetBoundaryIsRefused(t *testing.T) {
	// Arrange — the boundary oneof is the store's whole statement about older
	// history. An unset arm is a protocol violation, not a third answer.
	h := newLivePageHarness(t, pageTextEvents(t, 30))
	h.client.spy.override = &corev1.MessagePage{LastPageSeq: 7}

	// Act.
	_, err := h.m.FirstConversationHistoryPage(context.Background(), "r1", "ws")

	// Assert.
	if err == nil {
		t.Fatal("a page stating no boundary arm was served, so whether older history remains went unstated")
	}
}

func TestALiveWorkspacesNextPageWithNoReaderPositionIsRefused(t *testing.T) {
	// Arrange — the position layer is unchanged by the route: a reader with no
	// place is refused rather than quietly handed the tail.
	h := newLivePageHarness(t, pageTextEvents(t, 30))

	// Act.
	_, err := h.m.NextConversationHistoryPage(context.Background(), "r1", "ws")

	// Assert.
	if !errors.Is(err, ErrHistoryReaderPositionless) {
		t.Fatalf("NextConversationHistoryPage error = %v, want ErrHistoryReaderPositionless", err)
	}
	if got := h.client.requests(); len(got) != 0 {
		t.Fatalf("a refused next page still asked the shim for %d page(s)", len(got))
	}
}

func TestALiveWorkspacesPageRecordsTheReaderPositionUnderItsGeneration(t *testing.T) {
	// Arrange — the position stays in conversation_reader_position keyed by
	// (reader, workspace), carrying the generation it was established under.
	h := newLivePageHarness(t, pageTextEvents(t, 30))

	// Act.
	h.firstPage(t, "r1")

	// Assert.
	got, ok, err := h.positions.ConversationReaderPosition("r1", "ws")
	if err != nil || !ok {
		t.Fatalf("ConversationReaderPosition(r1, ws) = (%+v, %v, %v), want a recorded position", got, ok, err)
	}
	minted := h.client.spy.pages()[0].GetLastPageSeq()
	if got.BeforeSeq != minted {
		t.Fatalf("recorded before_seq = %d, want the store's own last_page_seq %d", got.BeforeSeq, minted)
	}
	if got.GenerationID != h.generationID(t) {
		t.Fatalf("recorded generation = %q, want the live controller's %q", got.GenerationID, h.generationID(t))
	}
}

func TestALiveWorkspacesRotatedGenerationDropsTheReaderPosition(t *testing.T) {
	// Arrange — a position established in a seq space the workspace no longer
	// runs in names nothing, so it is FORGOTTEN and the next page is refused.
	h := newLivePageHarness(t, pageTextEvents(t, 30))
	h.firstPage(t, "r1")
	if err := h.positions.SetConversationReaderPosition("r1", "ws", "a-retired-generation", 12); err != nil {
		t.Fatalf("SetConversationReaderPosition: %v", err)
	}

	// Act.
	_, err := h.m.NextConversationHistoryPage(context.Background(), "r1", "ws")

	// Assert.
	if !errors.Is(err, ErrHistoryReaderPositionless) {
		t.Fatalf("NextConversationHistoryPage error = %v, want ErrHistoryReaderPositionless", err)
	}
	if _, ok, _ := h.positions.ConversationReaderPosition("r1", "ws"); ok {
		t.Fatal("the position established under a retired generation survived the refusal")
	}
}

// compile-time proof the real shim client satisfies the page verb this route
// asks of a live session's own transport.
var _ interface {
	MessagePage(context.Context, shimclient.MessagePageAnchor) (*corev1.MessagePage, error)
} = (*shimclient.Client)(nil)
