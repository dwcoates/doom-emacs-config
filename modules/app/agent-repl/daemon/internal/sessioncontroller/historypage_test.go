package sessioncontroller

import (
	"context"
	"errors"
	"sync"
	"testing"

	frontendv1 "agentrepl/proto/frontend/v1"
	protocolv1 "agentrepl/proto/protocol/v1"

	"claude-repld/internal/ssm"
)

// THE CLIENT CANNOT NAME A POSITION, and every case here is one consequence of
// that.
//
// A first page resets the reader's place; a next page continues from a place
// only the daemon holds; a reader with no place is REFUSED rather than quietly
// handed the tail; and a generation change takes the place away so the refusal
// happens on its own.

// fakePositions is the SSM's reader-position store, keyed exactly as the real
// one is — per reader PER WORKSPACE — so the independence cases below are
// measuring the key rather than the fake.
type fakePositions struct {
	mu   sync.Mutex
	rows map[string]ssm.ConversationReaderPosition
	// setErr fails the write, which must fail the request: a page served under
	// a position that was never recorded would silently re-serve itself.
	setErr error
}

func positionKey(reader, workspace string) string { return reader + "\x1f" + workspace }

func (p *fakePositions) ConversationReaderPosition(reader, workspace string) (ssm.ConversationReaderPosition, bool, error) {
	p.mu.Lock()
	defer p.mu.Unlock()
	got, ok := p.rows[positionKey(reader, workspace)]
	return got, ok, nil
}

func (p *fakePositions) SetConversationReaderPosition(reader, workspace, generationID string, beforeSeq uint64) error {
	if p.setErr != nil {
		return p.setErr
	}
	p.mu.Lock()
	defer p.mu.Unlock()
	if p.rows == nil {
		p.rows = map[string]ssm.ConversationReaderPosition{}
	}
	p.rows[positionKey(reader, workspace)] = ssm.ConversationReaderPosition{GenerationID: generationID, BeforeSeq: beforeSeq}
	return nil
}

func (p *fakePositions) DropConversationReaderPosition(reader, workspace string) error {
	p.mu.Lock()
	defer p.mu.Unlock()
	delete(p.rows, positionKey(reader, workspace))
	return nil
}

// The fake SSM the harness injects must ALSO be the position store, exactly as
// the real one is: sessioncontroller asserts the capability off the configured
// SSM rather than taking a second injection point.
func (f *fakeApplier) ConversationReaderPosition(reader, workspace string) (ssm.ConversationReaderPosition, bool, error) {
	return f.positions().ConversationReaderPosition(reader, workspace)
}

func (f *fakeApplier) SetConversationReaderPosition(reader, workspace, generationID string, beforeSeq uint64) error {
	return f.positions().SetConversationReaderPosition(reader, workspace, generationID, beforeSeq)
}

func (f *fakeApplier) DropConversationReaderPosition(reader, workspace string) error {
	return f.positions().DropConversationReaderPosition(reader, workspace)
}

// positions lazily creates the applier's position store, so the capability is
// present on every fakeApplier exactly as it is on the real SSM.
func (f *fakeApplier) positions() *fakePositions {
	f.positionsOnce.Do(func() { f.readerPositions = &fakePositions{} })
	return f.readerPositions
}

// historyHarness is a pageHarness whose reader positions a test can inspect.
type historyHarness struct {
	*pageHarness
	positions *fakePositions
}

func newHistoryHarness(t *testing.T, events []*protocolv1.Event) *historyHarness {
	t.Helper()
	h := &historyHarness{pageHarness: newPageHarness(t, events)}
	h.positions = h.applier.positions()
	return h
}

// firstPage and nextPage are the two verbs, named as the wire names them.
func (h *historyHarness) firstPage(t *testing.T, reader string) *frontendv1.ConversationHistoryPage {
	t.Helper()
	page, err := h.m.FirstConversationHistoryPage(context.Background(), reader, "ws")
	if err != nil {
		t.Fatalf("FirstConversationHistoryPage(reader=%q): %v", reader, err)
	}
	return page
}

func (h *historyHarness) nextPage(t *testing.T, reader string) *frontendv1.ConversationHistoryPage {
	t.Helper()
	page, err := h.m.NextConversationHistoryPage(context.Background(), reader, "ws")
	if err != nil {
		t.Fatalf("NextConversationHistoryPage(reader=%q): %v", reader, err)
	}
	return page
}

// historyPageUUIDs reads the ten slots in order, which is also the assertion
// that they were filled in order.
func historyPageUUIDs(page *frontendv1.ConversationHistoryPage) []string {
	slots := []*frontendv1.Message{
		page.GetMessage_1(), page.GetMessage_2(), page.GetMessage_3(), page.GetMessage_4(), page.GetMessage_5(),
		page.GetMessage_6(), page.GetMessage_7(), page.GetMessage_8(), page.GetMessage_9(), page.GetMessage_10(),
	}
	var out []string
	for _, m := range slots {
		if m == nil {
			break
		}
		out = append(out, m.GetUuid())
	}
	return out
}

func TestAFirstPageResetsAnEstablishedReaderPosition(t *testing.T) {
	// Arrange — a reader that has paged back into the history holds a position
	// deep in it. FirstPageCmd is the recovery from every lost place, so it
	// must move that position to the tail rather than continue from it.
	h := newHistoryHarness(t, pageTextEvents(t, 30))
	h.firstPage(t, "r1")
	h.nextPage(t, "r1")
	deep, _, _ := h.positions.ConversationReaderPosition("r1", "ws")

	// Act.
	page := h.firstPage(t, "r1")

	// Assert — the page is the newest ten, and the stored position now
	// continues from THIS page rather than from the deeper one.
	got := historyPageUUIDs(page)
	if len(got) != 10 || got[9] != "u30" {
		t.Fatalf("first page items = %v, want the newest ten ending at u30", got)
	}
	reset, found, _ := h.positions.ConversationReaderPosition("r1", "ws")
	if !found {
		t.Fatal("first page left the reader with no position at all")
	}
	if reset.BeforeSeq <= deep.BeforeSeq {
		t.Fatalf("first page left before_seq=%d, want it reset ABOVE the deep position %d", reset.BeforeSeq, deep.BeforeSeq)
	}
}

func TestANextPageFromAReaderWithNoPositionIsRefused(t *testing.T) {
	// Arrange — a store full of history, and a reader that never asked for a
	// first page. Answering with the tail would turn a client bug into a silent
	// tail read that neither end records.
	h := newHistoryHarness(t, pageTextEvents(t, 30))

	// Act.
	page, err := h.m.NextConversationHistoryPage(context.Background(), "r1", "ws")

	// Assert.
	if err == nil {
		t.Fatalf("NextConversationHistoryPage with no position returned a page (%d slot(s)) instead of refusing", len(historyPageUUIDs(page)))
	}
	if !errors.Is(err, ErrHistoryReaderPositionless) {
		t.Fatalf("refusal = %v, want ErrHistoryReaderPositionless so the client knows to ask for the first page", err)
	}
}

func TestANextPageIsNotAnsweredWithTheTailWhenThePositionIsMissing(t *testing.T) {
	// Arrange — the same refusal, measured from the other side: nothing at all
	// is served. A tail read here is the exact defect the refusal exists to
	// make impossible.
	h := newHistoryHarness(t, pageTextEvents(t, 30))

	// Act.
	page, _ := h.m.NextConversationHistoryPage(context.Background(), "r1", "ws")

	// Assert.
	if page != nil {
		t.Fatalf("refused next page still carried a page with %d slot(s)", len(historyPageUUIDs(page)))
	}
}

func TestAGenerationChangeDropsThePositionSoTheNextPageIsRefused(t *testing.T) {
	// Arrange — a reader with an established position, and then the workspace
	// rotates onto a new controller generation. No fence crosses the wire; the
	// daemon holds both halves and invalidates its own record.
	h := newHistoryHarness(t, pageTextEvents(t, 30))
	h.firstPage(t, "r1")
	h.applier.current["ws"] = &frontendv1.WorkspaceState{
		Workspace: "ws", SessionId: "s1", ControllerGenerationId: "g2", Fence: ssm.Fence("s1", "g2"),
	}

	// Act.
	_, err := h.m.NextConversationHistoryPage(context.Background(), "r1", "ws")

	// Assert — refused, and the stale position is gone rather than merely
	// unused.
	if !errors.Is(err, ErrHistoryReaderPositionless) {
		t.Fatalf("next page across a generation change: err = %v, want ErrHistoryReaderPositionless", err)
	}
	if _, found, _ := h.positions.ConversationReaderPosition("r1", "ws"); found {
		t.Fatal("the position established under the retired generation survived the rotation")
	}
}

func TestReaderPositionsAreIndependentPerReader(t *testing.T) {
	// Arrange — two tabs on one workspace. One pages back; the other has only
	// ever seen the tail, and must not inherit the first one's depth.
	h := newHistoryHarness(t, pageTextEvents(t, 30))
	h.firstPage(t, "r1")
	h.firstPage(t, "r2")
	h.nextPage(t, "r1")

	// Act.
	page := h.nextPage(t, "r2")

	// Assert — r2's second page is the ten below the tail, not the twenty below
	// it.
	got := historyPageUUIDs(page)
	if len(got) != 10 || got[9] != "u20" {
		t.Fatalf("r2's next page = %v, want the ten ending at u20 (r1's depth must not leak)", got)
	}
}

func TestReaderPositionsAreIndependentPerWorkspace(t *testing.T) {
	// Arrange — one reader, two workspaces. A position is per reader PER
	// WORKSPACE, so a place established in one says nothing about the other.
	h := newHistoryHarness(t, pageTextEvents(t, 30))
	h.firstPage(t, "r1")
	h.nextPage(t, "r1")

	// Act — the same reader's place in a workspace it never opened.
	_, found, err := h.positions.ConversationReaderPosition("r1", "/other")
	if err != nil {
		t.Fatalf("ConversationReaderPosition: %v", err)
	}

	// Assert.
	if found {
		t.Fatal("the reader's place in ws leaked into a workspace it never read")
	}
}

func TestAFirstPageCarriesTheLiveJoinSeq(t *testing.T) {
	// Arrange — the newest event sits at seq 30. The client splices onto the
	// live push stream at the seq the page is current THROUGH, so the join is
	// gap-free by construction rather than by timing.
	h := newHistoryHarness(t, pageTextEvents(t, 30))

	// Act.
	page := h.firstPage(t, "r1")

	// Assert.
	if page.GetLiveJoinSeq() != 30 {
		t.Fatalf("first page live_join_seq = %d, want 30", page.GetLiveJoinSeq())
	}
}

func TestANextPageCarriesNoLiveJoinSeq(t *testing.T) {
	// Arrange — a next page is history and has no live edge. A non-zero mark
	// here would move a client's live join BACKWARDS.
	h := newHistoryHarness(t, pageTextEvents(t, 30))
	h.firstPage(t, "r1")

	// Act.
	page := h.nextPage(t, "r1")

	// Assert.
	if page.GetLiveJoinSeq() != 0 {
		t.Fatalf("next page live_join_seq = %d, want 0", page.GetLiveJoinSeq())
	}
}

func TestAShortPageFillsItsSlotsDenselyFromOne(t *testing.T) {
	// Arrange — a conversation with fewer messages than the page has slots.
	// message_k set while message_(k-1) is empty is a bug the wire shape cannot
	// prevent, so the producer is what must not commit it.
	h := newHistoryHarness(t, pageTextEvents(t, 3))

	// Act.
	page := h.firstPage(t, "r1")

	// Assert — the three occupy slots one through three, and slot four is the
	// first empty one.
	if page.GetMessage_1() == nil || page.GetMessage_2() == nil || page.GetMessage_3() == nil {
		t.Fatalf("short page left a hole below its last message: %v", historyPageUUIDs(page))
	}
	if page.GetMessage_4() != nil {
		t.Fatal("short page filled a slot beyond the messages it carried")
	}
}

func TestAHistoryPageIsOldestFirst(t *testing.T) {
	// Arrange — the storage side of this protocol is NEWEST first; the frontend
	// page is oldest first, so a frontend renders a paged message with the code
	// that renders a pushed one.
	h := newHistoryHarness(t, pageTextEvents(t, 12))

	// Act.
	page := h.firstPage(t, "r1")

	// Assert.
	got := historyPageUUIDs(page)
	if len(got) != 10 || got[0] != "u3" || got[9] != "u12" {
		t.Fatalf("first page items = %v, want u3..u12 oldest first", got)
	}
}

func TestAPageWhoseReaderPositionCannotBeRecordedIsRefused(t *testing.T) {
	// Arrange — the write of the position fails. Serving the page anyway would
	// leave the reader continuing from a place that was never stored, which
	// re-serves the same history silently on the next request.
	h := newHistoryHarness(t, pageTextEvents(t, 30))
	h.positions.setErr = errors.New("the state store is unwritable")

	// Act.
	_, err := h.m.FirstConversationHistoryPage(context.Background(), "r1", "ws")

	// Assert.
	if err == nil {
		t.Fatal("a first page whose position could not be recorded was served anyway")
	}
}

func TestAHistoryPageWithNoReaderIdentityIsRefused(t *testing.T) {
	// Arrange — a position is per reader per workspace, so an unidentified
	// reader has nowhere to be filed. The empty key would be inherited by the
	// next unidentified reader.
	h := newHistoryHarness(t, pageTextEvents(t, 30))

	// Act.
	_, err := h.m.FirstConversationHistoryPage(context.Background(), "", "ws")

	// Assert.
	if err == nil {
		t.Fatal("a first page with no reader identity was served")
	}
}

func TestAnEleventhMessageIsRefusedRatherThanTruncated(t *testing.T) {
	// Arrange — eleven messages for ten slots. Truncating would state the
	// page's width falsely and no client could tell.
	var messages []*frontendv1.Message
	for i := 0; i < historyPageSlots+1; i++ {
		messages = append(messages, &frontendv1.Message{Uuid: "u"})
	}

	// Act.
	err := fillHistoryPageSlots(&frontendv1.ConversationHistoryPage{}, messages)

	// Assert.
	if err == nil {
		t.Fatal("an eleventh message was accepted into a ten-slot page")
	}
}
