package sessioncontroller

import (
	"context"
	"errors"
	"fmt"
	"testing"

	corev1 "agentrepl/proto/agentshim/core/v1"
	datav1 "agentrepl/proto/agentshim/data/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/errclass"
	"claude-repld/internal/ssm"

	"google.golang.org/protobuf/types/known/anypb"
)

// THE COLD OPEN IS NOT A FULL REPLAY ANY MORE.
//
// Every case here is about one of the three promises a page makes that the
// from_seq=0 replay it replaces did not have to: it is BOUNDED (a limit that
// counts renderable things, and a ceiling a client cannot talk its way past),
// it is CONTINUABLE (a cursor that reads the page before, and a start arm that
// says there is no such page), and it SPLICES GAP-FREE onto the live stream
// (live_join_seq, which is the seq the client rejoins at rather than whatever
// seq the first delta happens to carry).

// pageHarness is a durableHarness whose workspace has an authoritative
// WorkspaceState, which is the identity a page's fence is measured against.
type pageHarness struct {
	*durableHarness
}

// newPageHarness arranges an UNWIRED workspace — the state every workspace is
// in after a daemon bounce, and the one a cold webview actually mounts
// against — holding the given store events.
func newPageHarness(t *testing.T, events []*corev1.Event) *pageHarness {
	t.Helper()
	h := &pageHarness{durableHarness: newDurableHarness(t, &durableHistorySpy{events: events})}
	// The Fence is stamped exactly as the mint stamps it (ssm.compositeWorkspaceState),
	// because it is the token the reader compares against and the one a client
	// was actually shown. A fixture that carried the two identities but no fence
	// would be describing a WorkspaceState the daemon never publishes.
	h.applier.current = map[string]*frontendv1.WorkspaceState{
		"ws": {Workspace: "ws", SessionId: "s1", ControllerGenerationId: "g1", Fence: ssm.Fence("s1", "g1")},
	}
	if len(events) > 0 {
		h.seq.SetLastSeq("s1", events[len(events)-1].GetSeq())
	}
	return h
}

// page asks for one page under the live identity.
func (h *pageHarness) page(t *testing.T, anchor PageAnchor) *frontendv1.ConversationPage {
	t.Helper()
	got, err := h.m.ConversationPage(context.Background(), "ws", ssm.Fence("s1", "g1"), anchor)
	if err != nil {
		t.Fatalf("ConversationPage(%+v): %v", anchor, err)
	}
	return got
}

// pageAssistantEvent is a vendor assistant record carrying `blocks`, at seq.
//
// The blocks matter to exactly one question this file asks: how many TOP-LEVEL
// items an event contributes. A record with a text block and a tool_use block
// is ONE feed item carrying its tool call inside it, not two.
func pageAssistantEvent(t *testing.T, seq uint64, uuid string, blocks []*datav1.ContentBlock) *corev1.Event {
	t.Helper()
	a, err := anypb.New(&datav1.ClaudeStreamMessage{
		Msg: &datav1.ClaudeStreamMessage_Assistant{Assistant: &datav1.AssistantMessage{
			Uuid:    uuid,
			Message: &datav1.ApiAssistantMessage{Content: blocks},
		}},
	})
	if err != nil {
		t.Fatalf("anypb.New: %v", err)
	}
	return &corev1.Event{SessionId: "vendor-uuid", Seq: seq, ProducedAtMs: int64(seq) * 1_000, Payload: &corev1.Event_Vendor{Vendor: a}}
}

func pageTextBlock(text string) *datav1.ContentBlock {
	return &datav1.ContentBlock{Block: &datav1.ContentBlock_Text{Text: &datav1.TextBlock{Text: text}}}
}

func pageToolUseBlock(id, name string) *datav1.ContentBlock {
	return &datav1.ContentBlock{Block: &datav1.ContentBlock_ToolUse{ToolUse: &datav1.ToolUseBlock{Id: id, Name: name}}}
}

// pageTextEvents is a run of `n` one-item events at seqs 1..n.
func pageTextEvents(t *testing.T, n int) []*corev1.Event {
	t.Helper()
	var out []*corev1.Event
	for i := 1; i <= n; i++ {
		out = append(out, pageAssistantEvent(t, uint64(i), fmt.Sprintf("u%d", i), []*datav1.ContentBlock{pageTextBlock(fmt.Sprintf("m%d", i))}))
	}
	return out
}

// itemUUIDs names what a page actually carried, which is what every ordering
// and boundary assertion below reads.
func pageItemUUIDs(page *frontendv1.ConversationPage) []string {
	var out []string
	for _, it := range page.GetItems() {
		out = append(out, it.GetUuid())
	}
	return out
}

// --- the tail page ----------------------------------------------------------

func TestATailPageServesTheNewestItemsOldestFirst(t *testing.T) {
	// Arrange — twenty items in the store, of which a cold open wants the last
	// three.
	h := newPageHarness(t, pageTextEvents(t, 20))

	// Act.
	page := h.page(t, PageAnchor{Tail: true, Limit: 3})

	// Assert — the newest three, in feed order.
	got := pageItemUUIDs(page)
	if len(got) != 3 || got[0] != "u18" || got[1] != "u19" || got[2] != "u20" {
		t.Fatalf("tail page items = %v, want [u18 u19 u20]", got)
	}
}

func TestATailPageJoinsTheLiveStreamAtItsNewestSeq(t *testing.T) {
	// Arrange — the page's newest item sits at seq 20. A client that stored
	// "whatever seq the first delta carries" instead would skip everything the
	// session produced between the mint and its subscribe.
	h := newPageHarness(t, pageTextEvents(t, 20))

	// Act.
	page := h.page(t, PageAnchor{Tail: true, Limit: 3})

	// Assert — the resync mark is INCLUSIVE, so rejoining at 20 re-delivers
	// item 20 (deduped by uuid) and everything above it.
	if page.GetLiveJoinSeq() != 20 {
		t.Fatalf("live_join_seq = %d, want 20", page.GetLiveJoinSeq())
	}
}

func TestABeforePageCarriesNoLiveJoinSeq(t *testing.T) {
	// Arrange — a load-more page is history; it has no live edge to join at,
	// and a non-zero mark here would move a client's cursor BACKWARDS.
	h := newPageHarness(t, pageTextEvents(t, 20))
	tail := h.page(t, PageAnchor{Tail: true, Limit: 3})

	// Act.
	older := h.page(t, PageAnchor{Cursor: tail.GetMore().GetCursor(), Limit: 3})

	// Assert.
	if older.GetLiveJoinSeq() != 0 {
		t.Fatalf("before page live_join_seq = %d, want 0", older.GetLiveJoinSeq())
	}
}

// --- boundary classification ------------------------------------------------

func TestConstituentsRideInsideTheirItemAndDoNotCountTowardTheLimit(t *testing.T) {
	// Arrange — three records, each ONE top-level item, and each carrying a
	// tool call inside it. A page that counted constituents would serve one
	// item for a limit of three.
	var events []*corev1.Event
	for i := 1; i <= 3; i++ {
		events = append(events, pageAssistantEvent(t, uint64(i), fmt.Sprintf("u%d", i), []*datav1.ContentBlock{
			pageTextBlock(fmt.Sprintf("m%d", i)),
			pageToolUseBlock(fmt.Sprintf("tool-%d", i), "Read"),
		}))
	}
	h := newPageHarness(t, events)

	// Act.
	page := h.page(t, PageAnchor{Tail: true, Limit: 3})

	// Assert — three renderable things, each still carrying its tool call.
	got := pageItemUUIDs(page)
	if len(got) != 3 {
		t.Fatalf("page items = %v, want 3 top-level items with their tool calls inside them", got)
	}
	blocks := page.GetItems()[0].GetAgent().GetResponse().GetBody().GetContent()
	if len(blocks) != 2 {
		t.Fatalf("first item carried %d content block(s), want the text and the tool_use it was emitted with", len(blocks))
	}
}

// --- continuation -----------------------------------------------------------

func TestAPageWithOlderHistoryHandsBackAContinuationCursor(t *testing.T) {
	// Arrange.
	h := newPageHarness(t, pageTextEvents(t, 20))

	// Act.
	page := h.page(t, PageAnchor{Tail: true, Limit: 3})

	// Assert.
	if page.GetMore().GetCursor() == "" {
		t.Fatalf("page continuation = %T, want a `more` arm with a cursor", page.GetContinuation())
	}
}

func TestTheContinuationCursorReadsThePageImmediatelyOlder(t *testing.T) {
	// Arrange — the tail page served u18..u20.
	h := newPageHarness(t, pageTextEvents(t, 20))
	tail := h.page(t, PageAnchor{Tail: true, Limit: 3})

	// Act.
	older := h.page(t, PageAnchor{Cursor: tail.GetMore().GetCursor(), Limit: 3})

	// Assert — the three immediately before, with no overlap and no gap.
	got := pageItemUUIDs(older)
	if len(got) != 3 || got[0] != "u15" || got[1] != "u16" || got[2] != "u17" {
		t.Fatalf("second page items = %v, want [u15 u16 u17]", got)
	}
}

func TestAPageThatReachesTheBeginningRetiresLoadMore(t *testing.T) {
	// Arrange — exactly three items exist, and the page asks for three. The
	// walk reaches the floor with nothing left behind, which is the one case
	// that is genuinely the beginning.
	h := newPageHarness(t, pageTextEvents(t, 3))

	// Act.
	page := h.page(t, PageAnchor{Tail: true, Limit: 3})

	// Assert.
	if page.GetStart() == nil {
		t.Fatalf("page continuation = %v, want the `start` arm so the client retires load-more", page.GetContinuation())
	}
}

func TestAnEmptyConversationPagesToTheBeginningWithNoItems(t *testing.T) {
	// Arrange — a workspace whose store holds nothing. Silence must be
	// reported as the empty conversation it is, never as a failure and never
	// as an endless load-more.
	h := newPageHarness(t, nil)

	// Act.
	page := h.page(t, PageAnchor{Tail: true, Limit: 10})

	// Assert.
	if len(page.GetItems()) != 0 {
		t.Fatalf("page items = %v, want none", pageItemUUIDs(page))
	}
	if page.GetStart() == nil {
		t.Fatalf("page continuation = %v, want the `start` arm", page.GetContinuation())
	}
	if page.GetLiveJoinSeq() != 0 {
		t.Fatalf("live_join_seq = %d, want 0", page.GetLiveJoinSeq())
	}
}

func TestPagingStopsAtTheNewestClearOrCompaction(t *testing.T) {
	// Arrange — a clear at seq 10 cut everything below it. History above a
	// clear is history a frontend discards, so paging into it would serve
	// pages nobody can see.
	h := newPageHarness(t, pageTextEvents(t, 12))
	h.floors.SetNewestClearOrCompactSeq("s1", 10)

	// Act.
	page := h.page(t, PageAnchor{Tail: true, Limit: 10})

	// Assert — only the items at or above the cut, and the cut IS the
	// beginning.
	got := pageItemUUIDs(page)
	if len(got) != 3 || got[0] != "u10" {
		t.Fatalf("page items = %v, want [u10 u11 u12]", got)
	}
	if page.GetStart() == nil {
		t.Fatalf("page continuation = %v, want the `start` arm: the clear is this conversation's beginning", page.GetContinuation())
	}
}

// --- the clamp --------------------------------------------------------------

func TestAnOversizedLimitIsClampedRatherThanServed(t *testing.T) {
	// Arrange — 80 items, and a client asking for all of them at once. That
	// request IS the full replay this path exists to end.
	h := newPageHarness(t, pageTextEvents(t, 80))

	// Act.
	page := h.page(t, PageAnchor{Tail: true, Limit: 5000})

	// Assert.
	if len(page.GetItems()) != pageMaxLimit {
		t.Fatalf("page served %d items for a limit of 5000, want the ceiling of %d", len(page.GetItems()), pageMaxLimit)
	}
}

func TestAZeroLimitTakesTheDaemonDefault(t *testing.T) {
	// Arrange.
	h := newPageHarness(t, pageTextEvents(t, 40))

	// Act.
	page := h.page(t, PageAnchor{Tail: true})

	// Assert.
	if len(page.GetItems()) != pageDefaultLimit {
		t.Fatalf("page served %d items for limit 0, want the default of %d", len(page.GetItems()), pageDefaultLimit)
	}
}

// --- the fence --------------------------------------------------------------

func TestAPageIsRefusedWhenTheEchoedFenceNamesASupersededGeneration(t *testing.T) {
	// Arrange — a tab that outlived a daemon bounce still echoes the
	// generation it was reading when it decided to ask.
	h := newPageHarness(t, pageTextEvents(t, 5))

	// Act.
	_, err := h.m.ConversationPage(context.Background(), "ws", ssm.Fence("s1", "g-retired"), PageAnchor{Tail: true, Limit: 3})

	// Assert — refused BEFORE any read, so a stale request can never
	// half-serve someone else's history.
	if !errors.Is(err, errclass.ErrSessionSuperseded) {
		t.Fatalf("ConversationPage error = %v, want ErrSessionSuperseded", err)
	}
	if replays := h.history.replays(); len(replays) != 0 {
		t.Fatalf("a refused page read %d range(s) from the store, want none", len(replays))
	}
}

func TestAPageStampsTheFenceItWasMintedUnder(t *testing.T) {
	// Arrange — the fence is what a client byte-compares before adopting the
	// page, so an unstamped page is one it must discard.
	h := newPageHarness(t, pageTextEvents(t, 5))

	// Act.
	page := h.page(t, PageAnchor{Tail: true, Limit: 3})

	// Assert — the workspace's PUBLISHED fence, byte for byte.
	if got, want := page.GetFence(), ssm.Fence("s1", "g1"); got != want {
		t.Fatalf("page fence = %q, want the workspace's published fence %q", got, want)
	}
}

func TestAnUngeneratedWorkspacesPageCarriesTheAbsentFenceItPublishes(t *testing.T) {
	// Arrange — the hibernated workspace a cold webview mounts against after a
	// daemon bounce. Its published fence is ABSENT, and a page that composed
	// `Fence(session, "")` instead would stamp a token the mint never produces:
	// the client byte-compares, disagrees, and discards the page it just asked
	// for, which is the blank feed one layer further out.
	h := newPageHarness(t, pageTextEvents(t, 3))
	h.applier.setCurrent("ws", &frontendv1.WorkspaceState{
		Workspace: "ws", SessionId: "s1", ControllerGenerationId: "", Fence: "",
	})

	// Act.
	page, err := h.m.ConversationPage(context.Background(), "ws", "", PageAnchor{Tail: true, Limit: 3})
	if err != nil {
		t.Fatalf("ConversationPage: %v", err)
	}

	// Assert.
	if got := page.GetFence(); got != "" {
		t.Fatalf("page fence = %q, want the absent fence the workspace publishes", got)
	}
}

func TestACursorMintedForAnotherConversationIsRefused(t *testing.T) {
	// Arrange — a cursor whose seq is expressed in a different session's seq
	// space. Serving it would page someone else's history at plausible-looking
	// coordinates.
	h := newPageHarness(t, pageTextEvents(t, 20))
	foreign := encodePageCursor(pageCursor{sessionID: "s-other", beforeSeq: 10})

	// Act.
	_, err := h.m.ConversationPage(context.Background(), "ws", ssm.Fence("s1", "g1"), PageAnchor{Cursor: foreign, Limit: 3})

	// Assert.
	if !errors.Is(err, ErrPageCursorUnreadable) {
		t.Fatalf("ConversationPage error = %v, want ErrPageCursorUnreadable", err)
	}
}

// --- the walk ---------------------------------------------------------------

func TestATailPageDoesNotScanTheWholeConversation(t *testing.T) {
	// Arrange — the whole point. A conversation far larger than one window,
	// asked for its last three items.
	h := newPageHarness(t, pageTextEvents(t, 4000))

	// Act.
	h.page(t, PageAnchor{Tail: true, Limit: 3})

	// Assert — ONE range read, and it STARTS near the tail rather than at the
	// beginning. The upper bound is deliberately open (a tail page is never
	// capped at the daemon's consumption mark), so what bounds the cost is
	// where the read begins: within one window of the conversation's end.
	replays := h.history.replays()
	if len(replays) != 1 {
		t.Fatalf("tail page issued %d range read(s), want exactly 1", len(replays))
	}
	if replays[0][1] != 0 {
		t.Fatalf("tail page read to_seq = %d, want 0: a tail page's upper bound is open", replays[0][1])
	}
	if from := replays[0][0]; from < 4000-pageInitialWindow {
		t.Fatalf("tail page read from_seq = %d, want it within one window (%d) of the conversation's end at 4000", from, pageInitialWindow)
	}
}

func TestTheWalkWidensUntilThePageIsFull(t *testing.T) {
	// Arrange — one item, sitting far below the tail, with a long stretch of
	// nothing renderable above it. The first window finds nothing, so the walk
	// has to widen rather than serve an empty page.
	events := []*corev1.Event{pageAssistantEvent(t, 1, "u1", []*datav1.ContentBlock{pageTextBlock("m1")})}
	h := newPageHarness(t, events)
	h.seq.SetLastSeq("s1", 3000)

	// Act.
	page := h.page(t, PageAnchor{Tail: true, Limit: 3})

	// Assert — the item is found, and it took more than one window to find it.
	if got := pageItemUUIDs(page); len(got) != 1 || got[0] != "u1" {
		t.Fatalf("page items = %v, want [u1]", got)
	}
	if replays := h.history.replays(); len(replays) < 2 {
		t.Fatalf("the walk issued %d range read(s), want it to have widened at least once", len(replays))
	}
}
