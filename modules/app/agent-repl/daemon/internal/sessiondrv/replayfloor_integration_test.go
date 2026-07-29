package sessiondrv

import (
	"context"
	"sync"
	"testing"

	corev1 "agentrepl/proto/agentshim/core/v1"
	datav1 "agentrepl/proto/agentshim/data/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/shimclient"

	"google.golang.org/protobuf/types/known/anypb"
)

// THE REPLAY FLOOR, END TO END, THROUGH A STORE THAT HONORS ITS OWN BOUNDS.
//
// The floor is only half a mechanism on its own: what a frontend actually
// receives is decided by the floor AND by the bound conversion each replay path
// applies to it. A core.v1 ReplayRequest's from_seq is an EXCLUSIVE lower bound
// while the live ring's is INCLUSIVE, so the two paths agree only because the
// store path converts. These cases assert the EFFECTIVE ITEM SET rather than
// the requested bound, against a store fake that serves exactly the range it
// was asked for — a fake that streamed its whole history regardless would make
// every "and nothing below it" assertion vacuous.

// --- a store that honors the wire's bounds ----------------------------------

// storeHistoryClient serves a canned conversation history the way the shim's
// throwaway store subscription does: from_seq EXCLUSIVE, to_seq EXCLUSIVE (0 =
// unbounded). Honoring the bounds is the entire point — this is the collaborator
// whose contract the daemon's conversion has to match.
type storeHistoryClient struct {
	fakeClient

	mu      sync.Mutex
	history []*corev1.Event
	calls   [][2]uint64
}

func (c *storeHistoryClient) Replay(_ context.Context, from, to uint64, _ uint32, onEvent func(*corev1.Event)) (shimclient.ReplayResult, error) {
	c.mu.Lock()
	c.calls = append(c.calls, [2]uint64{from, to})
	history := c.history
	c.mu.Unlock()
	var delivered uint64
	for _, ev := range history {
		seq := ev.GetSeq()
		if seq <= from {
			continue
		}
		if to != 0 && seq >= to {
			continue
		}
		onEvent(ev)
		delivered++
	}
	return shimclient.ReplayResult{Delivered: delivered}, nil
}

// --- harness ----------------------------------------------------------------

// floorHarness is one live driver over a caller-chosen client, with the floor
// store and the durable high-water mark directly reachable so a restarted
// daemon (empty ring, durable marks) can be modeled exactly.
type floorHarness struct {
	m      *Manager
	push   *fakePusher
	seq    *fakeSeqStore
	floors *fakeClearCompactStore
}

func newFloorHarness(t *testing.T, client sessionClient) *floorHarness {
	t.Helper()
	h := &floorHarness{
		push:   &fakePusher{},
		seq:    &fakeSeqStore{seq: map[string]uint64{}},
		floors: newFakeClearCompactStore(),
	}
	m, err := New(Config{
		Push:              h.push,
		SSM:               &fakeApplier{},
		Progress:          &fakeProgress{},
		Spawner:           &fakeSpawner{},
		Locator:           fakeLocator{m: map[string]string{"ws": "s1"}},
		SeqStore:          h.seq,
		ClearCompactStore: h.floors,
		ProtocolVersion:   "1",
		Source:            stubSource{},
		FileDiagnostics:   fakeFileDiagnosticPersister{},
		newClient:         func(shimclient.Config) sessionClient { return client },
	})
	if err != nil {
		t.Fatalf("New: %v", err)
	}
	t.Cleanup(m.Close)
	if err := m.Ensure("ws"); err != nil {
		t.Fatalf("Ensure: %v", err)
	}
	h.m = m
	return h
}

// consumer returns the live driver's consumer, the sink a shim's demux feeds.
func (h *floorHarness) consumer(t *testing.T) *consumer {
	t.Helper()
	d, err := h.m.existing("ws")
	if err != nil {
		t.Fatalf("existing: %v", err)
	}
	return d.consumer
}

// forget drops everything pushed so far, so an assertion sees the REPLAY alone
// rather than the live pushes that seeded the ring.
func (h *floorHarness) forget() {
	h.push.mu.Lock()
	h.push.convo = nil
	h.push.typing = nil
	h.push.heartbeats = nil
	h.push.mu.Unlock()
}

// --- local event builders ---------------------------------------------------

// historyAssistant is one renderable assistant message at seq, identified by
// uuid (which becomes the conversation item's uuid).
func historyAssistant(t *testing.T, seq uint64, uuid string) *corev1.Event {
	t.Helper()
	a, err := anypb.New(&datav1.ClaudeStreamMessage{
		Msg: &datav1.ClaudeStreamMessage_Assistant{Assistant: &datav1.AssistantMessage{
			Uuid: uuid,
			Message: &datav1.ApiAssistantMessage{
				Content: []*datav1.ContentBlock{{Block: &datav1.ContentBlock_Text{Text: &datav1.TextBlock{Text: "hi"}}}},
			},
		}},
	})
	if err != nil {
		t.Fatalf("anypb.New: %v", err)
	}
	return &corev1.Event{SessionId: "vendor-uuid", Seq: seq, Payload: &corev1.Event_Vendor{Vendor: a}}
}

// historyClear is a first-class ContextCleared at seq, whose dedup key is the
// item uuid a frontend reconciles on.
func historyClear(seq uint64, dedupKey string) *corev1.Event {
	return &corev1.Event{
		SessionId: "vendor-uuid", Seq: seq, DedupKey: dedupKey,
		Payload: &corev1.Event_ContextCleared{ContextCleared: &corev1.ContextCleared{}},
	}
}

// historyCompact is a first-class ContextCompacted at seq, carrying the summary
// that stands in for the history it discarded.
func historyCompact(seq uint64, dedupKey, summary string) *corev1.Event {
	return &corev1.Event{
		SessionId: "vendor-uuid", Seq: seq, DedupKey: dedupKey,
		Payload: &corev1.Event_ContextCompacted{ContextCompacted: &corev1.ContextCompacted{
			PreTokens: 180000, PostTokens: 24000, Summary: summary,
		}},
	}
}

// --- local assertions -------------------------------------------------------

// pushedItemUUIDs is the effective item set a frontend received, in push order.
// It is the one view both replay paths can be compared through.
func pushedItemUUIDs(p *fakePusher) []string {
	p.mu.Lock()
	defer p.mu.Unlock()
	var out []string
	for _, cd := range p.convo {
		for _, item := range cd.GetItems() {
			out = append(out, item.GetUuid())
		}
	}
	return out
}

// hasUUID reports whether the effective item set contains uuid.
func hasUUID(uuids []string, uuid string) bool {
	for _, got := range uuids {
		if got == uuid {
			return true
		}
	}
	return false
}

// compactSummaries lists the summary carried by every pushed compaction item.
func compactSummaries(p *fakePusher) []string {
	p.mu.Lock()
	defer p.mu.Unlock()
	var out []string
	for _, cd := range p.convo {
		for _, item := range cd.GetItems() {
			if arm, ok := item.GetItem().(*frontendv1.ConversationItem_ContextCompacted); ok {
				out = append(out, arm.ContextCompacted.GetSummary())
			}
		}
	}
	return out
}

// --- the store path ---------------------------------------------------------

func TestStorePathReplayIncludesTheClearAndNothingBeneathIt(t *testing.T) {
	// Arrange — a restarted daemon: the ring is empty, so EVERY replay goes
	// through the store, and the durable floor is all that stands between the
	// frontend and the history the clear discarded.
	client := &storeHistoryClient{history: []*corev1.Event{
		historyAssistant(t, 100, "u100"),
		historyAssistant(t, 200, "u200"),
		historyClear(300, "clear:u-cut"),
		historyAssistant(t, 400, "u400"),
	}}
	h := newFloorHarness(t, client)
	h.floors.SetNewestClearOrCompactSeq("s1", 300)
	h.seq.SetLastSeq("s1", 400)

	// Act — the frontend asks from the very beginning.
	if err := h.m.Resync("ws", 0); err != nil {
		t.Fatalf("Resync: %v", err)
	}

	// Assert — the clear itself came back, and nothing it discarded did.
	got := pushedItemUUIDs(h.push)
	if !hasUUID(got, "clear:u-cut") {
		t.Fatalf("items = %v, want the clear at the floor to be replayed", got)
	}
	if hasUUID(got, "u100") || hasUUID(got, "u200") {
		t.Fatalf("items = %v, want nothing below the clear at 300", got)
	}
}

func TestStorePathReplayIncludesTheCompactionSummaryAndNothingBeneathIt(t *testing.T) {
	// Arrange — a compaction discards its history just as a clear does, and the
	// summary is the ONLY thing that stands in for what it discarded, so it is
	// the one item a floored replay can least afford to omit.
	client := &storeHistoryClient{history: []*corev1.Event{
		historyAssistant(t, 100, "u100"),
		historyAssistant(t, 200, "u200"),
		historyCompact(300, "compact:b-1", "the story so far"),
		historyAssistant(t, 400, "u400"),
	}}
	h := newFloorHarness(t, client)
	h.floors.SetNewestClearOrCompactSeq("s1", 300)
	h.seq.SetLastSeq("s1", 400)

	// Act.
	if err := h.m.Resync("ws", 0); err != nil {
		t.Fatalf("Resync: %v", err)
	}

	// Assert.
	if got := compactSummaries(h.push); len(got) != 1 || got[0] != "the story so far" {
		t.Fatalf("compaction summaries = %v, want one %q", got, "the story so far")
	}
	if got := pushedItemUUIDs(h.push); hasUUID(got, "u100") || hasUUID(got, "u200") {
		t.Fatalf("items = %v, want nothing below the compaction at 300", got)
	}
}

func TestOnlyTheNewestOfSeveralCutsFloorsTheReplay(t *testing.T) {
	// Arrange — a conversation cut twice. The older cut is already below the
	// floor the newer one sets, so replaying it would hand the frontend a bubble
	// announcing a discard of history it is no longer being sent.
	h := newFloorHarness(t, &fakeClient{})
	cons := h.consumer(t)
	cons.Consume(historyAssistant(t, 50, "u50"))
	cons.Consume(historyClear(100, "clear:u-first"))
	cons.Consume(historyAssistant(t, 150, "u150"))
	cons.Consume(historyCompact(200, "compact:b-second", "the story so far"))
	cons.Consume(historyAssistant(t, 250, "u250"))
	h.forget()

	// Act.
	if err := h.m.Resync("ws", 0); err != nil {
		t.Fatalf("Resync: %v", err)
	}

	// Assert — the newest cut governs alone.
	got := pushedItemUUIDs(h.push)
	if !hasUUID(got, "compact:b-second") {
		t.Fatalf("items = %v, want the newest cut replayed", got)
	}
	if hasUUID(got, "clear:u-first") || hasUUID(got, "u50") || hasUUID(got, "u150") {
		t.Fatalf("items = %v, want nothing at or below the older cut", got)
	}
}

func TestAConversationWithNoCutReplaysItsWholeHistory(t *testing.T) {
	// Arrange — no clear and no compaction has ever been observed, so there is
	// nothing to floor at and the client's own mark stands. Flooring a
	// conversation that was never cut would silently truncate a complete history.
	client := &storeHistoryClient{history: []*corev1.Event{
		historyAssistant(t, 100, "u100"),
		historyAssistant(t, 200, "u200"),
		historyAssistant(t, 300, "u300"),
	}}
	h := newFloorHarness(t, client)
	h.seq.SetLastSeq("s1", 300)

	// Act.
	if err := h.m.Resync("ws", 0); err != nil {
		t.Fatalf("Resync: %v", err)
	}

	// Assert.
	got := pushedItemUUIDs(h.push)
	for _, want := range []string{"u100", "u200", "u300"} {
		if !hasUUID(got, want) {
			t.Fatalf("items = %v, want the whole history including %q", got, want)
		}
	}
}

// --- the two paths must agree -----------------------------------------------

func TestTheRingPathAndTheStorePathReplayTheSameItems(t *testing.T) {
	// Arrange — THE reconciliation. One path's lower bound is INCLUSIVE and the
	// other's is EXCLUSIVE, so the same conversation, the same floor and the same
	// request must still produce the same items — otherwise a frontend sees a
	// different conversation depending only on whether the daemon happened to
	// have restarted.
	history := func(t *testing.T) []*corev1.Event {
		return []*corev1.Event{
			historyAssistant(t, 100, "u100"),
			historyClear(200, "clear:u-cut"),
			historyAssistant(t, 300, "u300"),
		}
	}
	ring := newFloorHarness(t, &fakeClient{})
	cons := ring.consumer(t)
	for _, ev := range history(t) {
		cons.Consume(ev) // live: the ring holds it and the clear raises the floor
	}
	ring.forget()

	store := newFloorHarness(t, &storeHistoryClient{history: history(t)})
	store.floors.SetNewestClearOrCompactSeq("s1", 200)
	store.seq.SetLastSeq("s1", 300) // an empty ring, as a restart leaves

	// Act — the same request against each.
	if err := ring.m.Resync("ws", 0); err != nil {
		t.Fatalf("ring Resync: %v", err)
	}
	if err := store.m.Resync("ws", 0); err != nil {
		t.Fatalf("store Resync: %v", err)
	}

	// Assert.
	ringItems, storeItems := pushedItemUUIDs(ring.push), pushedItemUUIDs(store.push)
	if len(ringItems) != len(storeItems) {
		t.Fatalf("ring items = %v, store items = %v — the two paths must agree", ringItems, storeItems)
	}
	for i := range ringItems {
		if ringItems[i] != storeItems[i] {
			t.Fatalf("ring items = %v, store items = %v — the two paths must agree", ringItems, storeItems)
		}
	}
}

// --- regression guards ------------------------------------------------------

func TestAReplayedCutSurvivesTheStorePathWithNoFloorInPlay(t *testing.T) {
	// Arrange — the VENDOR-PAYLOAD FILTER regression, isolated from the floor.
	// The replay sink used to forward only vendor payloads, so a clear sitting in
	// re-pulled history was dropped on the floor before any frontend saw it. That
	// is exactly this case: a store whose history was ingested before this daemon
	// ever ran, so nothing raised a floor and the clear is just another event in
	// the range.
	client := &storeHistoryClient{history: []*corev1.Event{
		historyAssistant(t, 100, "u100"),
		historyClear(200, "clear:u-cut"),
		historyAssistant(t, 300, "u300"),
	}}
	h := newFloorHarness(t, client)
	h.seq.SetLastSeq("s1", 300)

	// Act.
	if err := h.m.Resync("ws", 0); err != nil {
		t.Fatalf("Resync: %v", err)
	}

	// Assert.
	if got := pushedItemUUIDs(h.push); !hasUUID(got, "clear:u-cut") {
		t.Fatalf("items = %v, want the clear carried through an unfloored replay", got)
	}
}

func TestEphemeralEventsAreNeverReplayed(t *testing.T) {
	// Arrange — EPHEMERAL events (ContentDelta, HeartbeatProgress) bypass the
	// store and carry no seq, yet the ring retains every event it is handed. A
	// replay that re-emitted them would re-run a live-typing animation for text
	// that finished arriving long ago, and would relay a heartbeat for a tool
	// that is no longer running.
	h := newFloorHarness(t, &fakeClient{})
	cons := h.consumer(t)
	cons.Consume(historyAssistant(t, 10, "u10"))
	cons.Consume(&corev1.Event{
		SessionId: "vendor-uuid",
		Payload: &corev1.Event_ContentDelta{ContentDelta: &corev1.ContentDelta{
			Uuid: "u10", Delta: &corev1.ContentDelta_Text{Text: "hi"},
		}},
	})
	cons.Consume(&corev1.Event{
		SessionId: "vendor-uuid",
		Payload:   &corev1.Event_HeartbeatProgress{HeartbeatProgress: &corev1.HeartbeatProgress{ToolUseId: "t1"}},
	})
	cons.Consume(historyAssistant(t, 20, "u20"))
	h.forget()

	// Act.
	if err := h.m.Resync("ws", 0); err != nil {
		t.Fatalf("Resync: %v", err)
	}

	// Assert — the two persistent messages and nothing else.
	if got := pushedItemUUIDs(h.push); len(got) != 2 || got[0] != "u10" || got[1] != "u20" {
		t.Fatalf("items = %v, want exactly [u10 u20]", got)
	}
	h.push.mu.Lock()
	defer h.push.mu.Unlock()
	if len(h.push.typing) != 0 || len(h.push.heartbeats) != 0 {
		t.Fatalf("replay emitted %d typing and %d heartbeat relay(s), want none",
			len(h.push.typing), len(h.push.heartbeats))
	}
}
