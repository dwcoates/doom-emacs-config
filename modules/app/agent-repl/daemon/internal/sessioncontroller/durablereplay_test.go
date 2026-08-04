package sessioncontroller

import (
	"context"
	"errors"
	"fmt"
	"strings"
	"sync"
	"testing"

	corev1 "agentrepl/proto/agentshim/core/v1"
	datav1 "agentrepl/proto/agentshim/data/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/shimclient"
	"claude-repld/internal/storehistory"

	"google.golang.org/protobuf/types/known/anypb"
)

// A RESYNC IS SERVED WHENEVER A FRONTEND ASKS FOR ONE.
//
// A workspace with no live session controller used to have its conversation
// replay skipped, which rendered a correct footer over an empty feed after a
// daemon bounce. Its history is served from the durable store instead, through
// the same translation, curation, and provenance path the live replay uses.

// durableHistorySpy is a DurableHistorySource over a canned event list. It
// applies the same bounds the real store does, so a test asserting on the
// bounds is also asserting they MEAN something.
type durableHistorySpy struct {
	mu sync.Mutex
	// calls records one entry per replay, as [fromSeq, toSeq].
	calls [][2]uint64
	// caps records the max_events each replay carried.
	caps      []uint32
	events    []*corev1.Event
	err       error
	truncated string
}

func (s *durableHistorySpy) ReplayHistory(_ context.Context, _, _ string, fromSeq, toSeq uint64, maxEvents uint32, onEvent func(*corev1.Event)) (storehistory.Result, error) {
	s.mu.Lock()
	s.calls = append(s.calls, [2]uint64{fromSeq, toSeq})
	s.caps = append(s.caps, maxEvents)
	events, err, truncated := s.events, s.err, s.truncated
	s.mu.Unlock()
	if err != nil {
		return storehistory.Result{}, err
	}
	var res storehistory.Result
	for _, ev := range events {
		if ev.GetSeq() <= fromSeq {
			continue
		}
		if toSeq > 0 && ev.GetSeq() >= toSeq {
			break
		}
		if res.Delivered == 0 {
			res.FirstSeq = ev.GetSeq()
		}
		res.Delivered++
		res.LastSeq = ev.GetSeq()
		onEvent(ev)
	}
	if truncated != "" {
		res.Truncated, res.Reason = true, truncated
	}
	return res, nil
}

func (s *durableHistorySpy) replays() [][2]uint64 {
	s.mu.Lock()
	defer s.mu.Unlock()
	return append([][2]uint64(nil), s.calls...)
}

// durableHarness is a Manager whose "ws" workspace has NO live session
// controller — the state every workspace is in after a daemon bounce.
type durableHarness struct {
	m       *Manager
	push    *fakePusher
	applier *fakeApplier
	seq     *fakeSeqStore
	floors  *fakeClearCompactStore
	history *durableHistorySpy
	// receipts is the durable prompt-receipt ledger this harness's Manager
	// records into and replays from (promptreceipt_test.go).
	receipts *fakeReceiptStore
	logMu    *sync.Mutex
	logged   *[]string
}

func newDurableHarness(t *testing.T, history DurableHistorySource) *durableHarness {
	t.Helper()
	var logged []string
	var mu sync.Mutex
	h := &durableHarness{
		push:     &fakePusher{},
		applier:  &fakeApplier{},
		seq:      &fakeSeqStore{seq: map[string]uint64{}},
		floors:   newFakeClearCompactStore(),
		receipts: newFakeReceiptStore(),
		logMu:    &mu,
		logged:   &logged,
	}
	if spy, ok := history.(*durableHistorySpy); ok {
		h.history = spy
	}
	m, err := New(Config{
		Push:              h.push,
		SSM:               h.applier,
		Spawner:           &fakeSpawner{},
		Locator:           fakeLocator{m: map[string]string{"ws": "s1"}},
		SeqStore:          h.seq,
		ClearCompactStore: h.floors,
		TurnAccountings:   emptyTurnAccountingStore{},
		DurableHistory:    history,
		PromptReceipts:    h.receipts,
		ProtocolVersion:   "1",
		Source:            stubSource{},
		FileDiagnostics:   fakeFileDiagnosticPersister{},
		Logf: func(f string, a ...any) {
			mu.Lock()
			logged = append(logged, fmt.Sprintf(f, a...))
			mu.Unlock()
		},
		newClient: func(shimclient.Config) sessionClient { return &replayClient{} },
	})
	if err != nil {
		t.Fatalf("New: %v", err)
	}
	t.Cleanup(m.Close)
	h.m = m
	return h
}

// durableAssistantEvent is a vendor assistant message at seq, stamped at tsMs
// so the lease ledger has an instant to place it against.
func durableAssistantEvent(t *testing.T, seq uint64, uuid string, tsMs int64) *corev1.Event {
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
	return &corev1.Event{SessionId: "vendor-uuid", Seq: seq, ProducedAtMs: tsMs, Payload: &corev1.Event_Vendor{Vendor: a}}
}

func (h *durableHarness) throughSeqs() []uint64 {
	h.push.mu.Lock()
	defer h.push.mu.Unlock()
	var out []uint64
	for _, cd := range h.push.convo {
		out = append(out, cd.GetThroughSeq())
	}
	return out
}

func (h *durableHarness) logLines() []string {
	h.logMu.Lock()
	defer h.logMu.Unlock()
	return append([]string(nil), *h.logged...)
}

// --- the replay itself ------------------------------------------------------

func TestAnUnwiredWorkspaceServesItsStoredConversation(t *testing.T) {
	// Arrange — the daemon bounced: nothing is live, and the whole
	// conversation is sitting in the store.
	history := &durableHistorySpy{events: []*corev1.Event{
		durableAssistantEvent(t, 1, "u1", 1_000),
		durableAssistantEvent(t, 2, "u2", 2_000),
		durableAssistantEvent(t, 3, "u3", 3_000),
	}}
	h := newDurableHarness(t, history)

	// Act.
	if err := h.m.Resync("ws", 0); err != nil {
		t.Fatalf("Resync: %v", err)
	}

	// Assert — the full history reached the frontend push, in store order.
	got := h.throughSeqs()
	if len(got) != 3 || got[0] != 1 || got[1] != 2 || got[2] != 3 {
		t.Fatalf("pushed through_seqs = %v, want [1 2 3]", got)
	}
}

func TestADurableReplayStartsAtTheClientsOwnMark(t *testing.T) {
	// Arrange — a frontend holding seq 5 asks for everything after it. The
	// store's from_seq is EXCLUSIVE, so the inclusive mark 5 goes down as 4.
	history := &durableHistorySpy{events: []*corev1.Event{
		durableAssistantEvent(t, 5, "u5", 5_000),
		durableAssistantEvent(t, 6, "u6", 6_000),
	}}
	h := newDurableHarness(t, history)
	h.seq.SetLastSeq("s1", 20)

	// Act.
	if err := h.m.Resync("ws", 5); err != nil {
		t.Fatalf("Resync: %v", err)
	}

	// Assert.
	if got := h.history.replays(); len(got) != 1 || got[0][0] != 4 {
		t.Fatalf("durable replay bounds = %v, want one replay from_seq=4", got)
	}
}

func TestADurableReplayIsFlooredAtTheNewestClearOrCompaction(t *testing.T) {
	// Arrange — a clear at seq 30 discarded everything below it, so a client
	// mark of 1 must not drag that history back onto the screen.
	history := &durableHistorySpy{events: []*corev1.Event{durableAssistantEvent(t, 31, "u31", 31_000)}}
	h := newDurableHarness(t, history)
	h.seq.SetLastSeq("s1", 60)
	h.floors.SetNewestClearOrCompactSeq("s1", 30)

	// Act.
	if err := h.m.Resync("ws", 1); err != nil {
		t.Fatalf("Resync: %v", err)
	}

	// Assert — INCLUSIVE of the clear itself, so the exclusive bound is 29.
	if got := h.history.replays(); len(got) != 1 || got[0][0] != 29 {
		t.Fatalf("durable replay bounds = %v, want one replay from_seq=29", got)
	}
}

func TestADurableReplayHasNoUpperBound(t *testing.T) {
	// Arrange — an unwired workspace holds no retained ring, so there is no
	// live window for the replay to stop short of.
	history := &durableHistorySpy{events: []*corev1.Event{durableAssistantEvent(t, 1, "u1", 1_000)}}
	h := newDurableHarness(t, history)

	// Act.
	if err := h.m.Resync("ws", 0); err != nil {
		t.Fatalf("Resync: %v", err)
	}

	// Assert.
	if got := h.history.replays(); len(got) != 1 || got[0][1] != 0 {
		t.Fatalf("durable replay bounds = %v, want one replay to_seq=0", got)
	}
}

// --- provenance -------------------------------------------------------------

func TestADurableReplayStampsMergeProvenanceFromTheLeaseLedger(t *testing.T) {
	// Arrange — the item was produced inside a merge window that has since
	// CLOSED. The durable ledger still says so, and a replay must agree.
	history := &durableHistorySpy{events: []*corev1.Event{durableAssistantEvent(t, 7, "u7", 5_000)}}
	h := newDurableHarness(t, history)
	h.applier.mergeWindows = map[string][][2]int64{"ws": {{4_000, 6_000}}}

	// Act.
	if err := h.m.Resync("ws", 0); err != nil {
		t.Fatalf("Resync: %v", err)
	}

	// Assert.
	h.push.mu.Lock()
	defer h.push.mu.Unlock()
	if len(h.push.convo) != 1 {
		t.Fatalf("pushed %d deltas, want 1", len(h.push.convo))
	}
	if got := h.push.convo[0].GetItems()[0].GetSource(); got != frontendv1.ConversationSource_CONVERSATION_SOURCE_MERGE {
		t.Fatalf("replayed item source = %v, want CONVERSATION_SOURCE_MERGE", got)
	}
}

func TestADurableReplayStampsUserProvenanceOutsideEveryMergeWindow(t *testing.T) {
	// Arrange — the same path must not rewrite a user's history as a merge's.
	history := &durableHistorySpy{events: []*corev1.Event{durableAssistantEvent(t, 7, "u7", 9_000)}}
	h := newDurableHarness(t, history)
	h.applier.mergeWindows = map[string][][2]int64{"ws": {{4_000, 6_000}}}

	// Act.
	if err := h.m.Resync("ws", 0); err != nil {
		t.Fatalf("Resync: %v", err)
	}

	// Assert.
	h.push.mu.Lock()
	defer h.push.mu.Unlock()
	if got := h.push.convo[0].GetItems()[0].GetSource(); got != frontendv1.ConversationSource_CONVERSATION_SOURCE_USER {
		t.Fatalf("replayed item source = %v, want CONVERSATION_SOURCE_USER", got)
	}
}

// --- failures are loud ------------------------------------------------------

func TestAnUnreadableStoreFailsTheResync(t *testing.T) {
	// Arrange — a frontend cannot tell silence from an empty conversation, so
	// an unreadable store must nack rather than render nothing.
	history := &durableHistorySpy{err: errors.New("dial unix store.sock: connection refused")}
	h := newDurableHarness(t, history)

	// Act.
	err := h.m.Resync("ws", 0)

	// Assert.
	if err == nil {
		t.Fatal("an unreadable store served a silently empty resync")
	}
	if !strings.Contains(err.Error(), "connection refused") {
		t.Fatalf("resync error = %v, want it to carry the store's cause", err)
	}
}

func TestAnUnreadableStoreIsLoggedWithItsCause(t *testing.T) {
	// Arrange.
	history := &durableHistorySpy{err: errors.New("dial unix store.sock: connection refused")}
	h := newDurableHarness(t, history)

	// Act.
	_ = h.m.Resync("ws", 0)

	// Assert.
	for _, l := range h.logLines() {
		if strings.Contains(l, "durable resync FAILED") && strings.Contains(l, "connection refused") {
			return
		}
	}
	t.Fatalf("no loud durable-resync failure line; lines=%v", h.logLines())
}

func TestATruncatedDurableReplayIsReportedRatherThanPresentedAsComplete(t *testing.T) {
	// Arrange — a partial answer is reported as one.
	history := &durableHistorySpy{
		events:    []*corev1.Event{durableAssistantEvent(t, 1, "u1", 1_000)},
		truncated: "the store closed the subscription",
	}
	h := newDurableHarness(t, history)

	// Act.
	err := h.m.Resync("ws", 0)

	// Assert.
	if !errors.Is(err, ErrRepullTruncated) {
		t.Fatalf("resync error = %v, want ErrRepullTruncated", err)
	}
}

func TestAResyncWithNoDurableSourceWiredFailsLoudly(t *testing.T) {
	// Arrange — nothing can read the workspace's history, and saying nothing
	// would render the empty feed this path exists to close.
	h := newDurableHarness(t, nil)

	// Act.
	err := h.m.Resync("ws", 0)

	// Assert.
	if err == nil {
		t.Fatal("a resync with no durable history source wired returned no error")
	}
}

func TestAResyncForAWorkspaceWithNoSessionRecordFailsLoudly(t *testing.T) {
	// Arrange — no record means no conversation to locate in the store.
	history := &durableHistorySpy{}
	h := newDurableHarness(t, history)

	// Act.
	err := h.m.Resync("/unknown", 0)

	// Assert.
	if err == nil {
		t.Fatal("a resync for a workspace with no session record returned no error")
	}
	if len(h.history.replays()) != 0 {
		t.Fatalf("the store was read for a workspace with no session record: %v", h.history.replays())
	}
}

// --- the live path is untouched ---------------------------------------------

func TestALiveWorkspaceStillResyncsFromItsRetainedRing(t *testing.T) {
	// Arrange — a workspace WITH a live session controller keeps the ring
	// path: the durable route is for the unwired case only.
	history := &durableHistorySpy{events: []*corev1.Event{durableAssistantEvent(t, 1, "u1", 1_000)}}
	h := newDurableHarness(t, history)
	if err := h.m.Ensure("ws"); err != nil {
		t.Fatalf("Ensure: %v", err)
	}
	d, err := h.m.existing("ws")
	if err != nil {
		t.Fatalf("existing: %v", err)
	}
	d.consumer.Consume(durableAssistantEvent(t, 10, "u10", 10_000))

	// Act — entirely within the ring.
	if err := h.m.Resync("ws", 10); err != nil {
		t.Fatalf("Resync: %v", err)
	}

	// Assert.
	if got := h.history.replays(); len(got) != 0 {
		t.Fatalf("a live workspace read the durable store: %v", got)
	}
}
