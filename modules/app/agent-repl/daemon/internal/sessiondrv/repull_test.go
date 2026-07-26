package sessiondrv

import (
	"context"
	"errors"
	"strings"
	"sync"
	"testing"

	corev1 "agentrepl/proto/agentshim/core/v1"
	datav1 "agentrepl/proto/agentshim/data/v1"

	"claude-repld/internal/shimclient"

	"google.golang.org/protobuf/types/known/anypb"
)

// --- fakes ------------------------------------------------------------------

// fakeHistory records what the driver asked the store for and replays a
// scripted slice back through the caller's sink.
type fakeHistory struct {
	mu sync.Mutex
	// calls records one entry per Replay, as "from-stop".
	calls [][2]uint64
	// vendorIDs records the id each Replay subscribed under.
	vendorIDs []string
	events    []*corev1.Event
	err       error
	// block, when non-nil, holds Replay open until it is closed — the
	// in-flight-re-pull shape the concurrency guard is about.
	block chan struct{}
}

func (h *fakeHistory) Replay(_ context.Context, vendorID string, from, stop uint64, onEvent func(*corev1.Event)) (int, error) {
	h.mu.Lock()
	h.calls = append(h.calls, [2]uint64{from, stop})
	h.vendorIDs = append(h.vendorIDs, vendorID)
	block := h.block
	events := h.events
	err := h.err
	h.mu.Unlock()
	if block != nil {
		<-block
	}
	for _, ev := range events {
		onEvent(ev)
	}
	return len(events), err
}

func (h *fakeHistory) callCount() int {
	h.mu.Lock()
	defer h.mu.Unlock()
	return len(h.calls)
}

// repullHarness is one live driver whose collaborators are all recorders, so a
// re-pull's blast radius is directly assertable.
type repullHarness struct {
	m        *Manager
	push     *fakePusher
	applier  *fakeApplier
	progress *fakeProgress
	history  *fakeHistory
	seq      *fakeSeqStore
}

func newRepullHarness(t *testing.T, history *fakeHistory, vendorID string) *repullHarness {
	t.Helper()
	h := &repullHarness{
		push:     &fakePusher{},
		applier:  &fakeApplier{},
		progress: &fakeProgress{},
		history:  history,
		seq:      &fakeSeqStore{seq: map[string]uint64{}},
	}
	m, err := New(Config{
		Push:            h.push,
		SSM:             h.applier,
		Progress:        h.progress,
		Spawner:         &fakeSpawner{},
		Locator:         fakeLocator{m: map[string]string{"ws": "s1"}},
		SeqStore:        h.seq,
		ProtocolVersion: "1",
		Source:          stubSource{},
		History:         history,
		VendorSessionID: func(string) string { return vendorID },
		newClient:       func(cfg shimclient.Config) sessionClient { return &fakeClient{cfg: cfg} },
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

// driver returns the live driver for "ws".
func (h *repullHarness) driver(t *testing.T) *driven {
	t.Helper()
	d, err := h.m.existing("ws")
	if err != nil {
		t.Fatalf("existing: %v", err)
	}
	return d
}

// assistantEvent is a vendor event carrying one renderable assistant message.
func assistantEvent(t *testing.T, seq uint64, uuid string) *corev1.Event {
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

// --- tests ------------------------------------------------------------------

func TestResyncWithinTheRingDoesNotRePull(t *testing.T) {
	// Arrange — the ring's floor is seq 10.
	history := &fakeHistory{}
	h := newRepullHarness(t, history, "vendor-uuid")
	h.driver(t).consumer.Consume(assistantEvent(t, 10, "u10"))
	// Act
	if err := h.m.Resync("ws", 10); err != nil {
		t.Fatalf("Resync: %v", err)
	}
	// Assert
	if history.callCount() != 0 {
		t.Fatalf("re-pulled %d time(s) for an in-window resync, want 0", history.callCount())
	}
}

func TestResyncBelowTheRingFloorRePullsTheGap(t *testing.T) {
	// Arrange — the ring's floor is seq 10; the frontend asks from 0.
	history := &fakeHistory{}
	h := newRepullHarness(t, history, "vendor-uuid")
	h.driver(t).consumer.Consume(assistantEvent(t, 10, "u10"))
	// Act
	if err := h.m.Resync("ws", 0); err != nil {
		t.Fatalf("Resync: %v", err)
	}
	// Assert
	history.mu.Lock()
	defer history.mu.Unlock()
	if len(history.calls) != 1 || history.calls[0] != [2]uint64{0, 10} {
		t.Fatalf("re-pull calls = %v, want one [0 10]", history.calls)
	}
}

func TestEmptyRingTakesItsFloorFromTheDurableHighWaterMark(t *testing.T) {
	// Arrange — a restarted daemon: nothing retained, but last_seen_seq survives.
	history := &fakeHistory{}
	h := newRepullHarness(t, history, "vendor-uuid")
	h.seq.SetLastSeq("s1", 7117)
	// Act
	if err := h.m.Resync("ws", 0); err != nil {
		t.Fatalf("Resync: %v", err)
	}
	// Assert
	history.mu.Lock()
	defer history.mu.Unlock()
	if len(history.calls) != 1 || history.calls[0] != [2]uint64{0, 7118} {
		t.Fatalf("re-pull calls = %v, want one [0 7118]", history.calls)
	}
}

func TestRePullSubscribesUnderTheVendorSessionID(t *testing.T) {
	// Arrange
	history := &fakeHistory{}
	h := newRepullHarness(t, history, "vendor-uuid")
	h.seq.SetLastSeq("s1", 5)
	// Act
	if err := h.m.Resync("ws", 0); err != nil {
		t.Fatalf("Resync: %v", err)
	}
	// Assert
	history.mu.Lock()
	defer history.mu.Unlock()
	if len(history.vendorIDs) != 1 || history.vendorIDs[0] != "vendor-uuid" {
		t.Fatalf("re-pull subscribed under %v, want [vendor-uuid]", history.vendorIDs)
	}
}

func TestRePulledEventsReachConversation(t *testing.T) {
	// Arrange
	history := &fakeHistory{events: []*corev1.Event{assistantEvent(t, 3, "old")}}
	h := newRepullHarness(t, history, "vendor-uuid")
	h.seq.SetLastSeq("s1", 9)
	// Act
	if err := h.m.Resync("ws", 0); err != nil {
		t.Fatalf("Resync: %v", err)
	}
	// Assert
	h.push.mu.Lock()
	defer h.push.mu.Unlock()
	if len(h.push.convo) != 1 || h.push.convo[0].GetItems()[0].GetUuid() != "old" {
		t.Fatalf("conversation pushes = %v, want one item uuid=old", h.push.convo)
	}
}

func TestRePulledEventsNeverReachTheSSM(t *testing.T) {
	// Arrange — the SSM consumed this history once already; re-applying it is
	// what drives live_task_count into impossible values.
	history := &fakeHistory{events: []*corev1.Event{
		{SessionId: "vendor-uuid", Seq: 3, Payload: &corev1.Event_TaskEnded{TaskEnded: &corev1.TaskEnded{TaskId: "t1"}}},
		assistantEvent(t, 4, "old"),
	}}
	h := newRepullHarness(t, history, "vendor-uuid")
	h.seq.SetLastSeq("s1", 9)
	// Act
	if err := h.m.Resync("ws", 0); err != nil {
		t.Fatalf("Resync: %v", err)
	}
	// Assert
	if len(h.applier.applied) != 0 {
		t.Fatalf("SSM saw %d re-pulled event(s), want 0", len(h.applier.applied))
	}
}

func TestRePulledEventsNeverReachTheProgressResolver(t *testing.T) {
	// Arrange
	history := &fakeHistory{events: []*corev1.Event{assistantEvent(t, 3, "old")}}
	h := newRepullHarness(t, history, "vendor-uuid")
	h.seq.SetLastSeq("s1", 9)
	// Act
	if err := h.m.Resync("ws", 0); err != nil {
		t.Fatalf("Resync: %v", err)
	}
	// Assert
	if len(h.progress.applied) != 0 {
		t.Fatalf("progress saw %d re-pulled event(s), want 0", len(h.progress.applied))
	}
}

func TestRePulledEventsNeverRebuildTheTaskCatalog(t *testing.T) {
	// Arrange — a historical task lifecycle must not repopulate the roster.
	history := &fakeHistory{events: []*corev1.Event{
		{SessionId: "vendor-uuid", Seq: 3, Payload: &corev1.Event_TaskStarted{TaskStarted: &corev1.TaskStarted{TaskId: "t1"}}},
	}}
	h := newRepullHarness(t, history, "vendor-uuid")
	h.seq.SetLastSeq("s1", 9)
	// Act
	if err := h.m.Resync("ws", 0); err != nil {
		t.Fatalf("Resync: %v", err)
	}
	// Assert
	h.push.mu.Lock()
	defer h.push.mu.Unlock()
	if len(h.push.catalog) != 0 {
		t.Fatalf("re-pull pushed %d task catalog(s), want 0", len(h.push.catalog))
	}
}

func TestRePulledEventsNeverEnterTheRetainedRing(t *testing.T) {
	// Arrange — back-filling the live window would drift the floor under the
	// next request.
	history := &fakeHistory{events: []*corev1.Event{assistantEvent(t, 3, "old")}}
	h := newRepullHarness(t, history, "vendor-uuid")
	h.seq.SetLastSeq("s1", 9)
	// Act
	if err := h.m.Resync("ws", 0); err != nil {
		t.Fatalf("Resync: %v", err)
	}
	// Assert
	if got := len(h.driver(t).consumer.snapshotRing()); got != 0 {
		t.Fatalf("ring holds %d re-pulled event(s), want 0", got)
	}
}

func TestConcurrentCoveredRePullCoalesces(t *testing.T) {
	// Arrange — a pull from 0 is already running when a second asks from 5.
	history := &fakeHistory{block: make(chan struct{})}
	h := newRepullHarness(t, history, "vendor-uuid")
	h.seq.SetLastSeq("s1", 100)
	started := make(chan struct{})
	go func() {
		close(started)
		_ = h.m.Resync("ws", 0)
	}()
	<-started
	waitFor(t, "the first re-pull to start", func() bool { return history.callCount() == 1 })
	// Act
	err := h.m.Resync("ws", 5)
	close(history.block)
	// Assert
	if err != nil {
		t.Fatalf("a covered concurrent resync must coalesce, got %v", err)
	}
}

func TestConcurrentUncoveredRePullIsRefusedLoudly(t *testing.T) {
	// Arrange — a pull from 50 is running when a second asks from 5, which it
	// does NOT cover.
	history := &fakeHistory{block: make(chan struct{})}
	h := newRepullHarness(t, history, "vendor-uuid")
	h.seq.SetLastSeq("s1", 100)
	started := make(chan struct{})
	go func() {
		close(started)
		_ = h.m.Resync("ws", 50)
	}()
	<-started
	waitFor(t, "the first re-pull to start", func() bool { return history.callCount() == 1 })
	// Act
	err := h.m.Resync("ws", 5)
	close(history.block)
	// Assert
	if !errors.Is(err, ErrRepullInFlight) {
		t.Fatalf("err = %v, want ErrRepullInFlight", err)
	}
}

func TestBelowFloorResyncWithNoHistorySourceFailsLoudly(t *testing.T) {
	// Arrange
	m, _ := newTestManager(t, fakeLocator{m: map[string]string{"ws": "s1"}}, &fakeSpawner{})
	if err := m.Ensure("ws"); err != nil {
		t.Fatalf("Ensure: %v", err)
	}
	// Act
	err := m.Resync("ws", 0)
	// Assert
	if err == nil || !strings.Contains(err.Error(), "no history source is wired") {
		t.Fatalf("err = %v, want a loud no-history-source failure", err)
	}
}

func TestBelowFloorResyncWithNoVendorSessionIDFailsLoudly(t *testing.T) {
	// Arrange — the store keys events by the vendor uuid; without one a
	// subscribe registers on a channel nothing publishes to.
	history := &fakeHistory{}
	h := newRepullHarness(t, history, "")
	h.seq.SetLastSeq("s1", 9)
	// Act
	err := h.m.Resync("ws", 0)
	// Assert
	if err == nil || !strings.Contains(err.Error(), "no vendor session id") {
		t.Fatalf("err = %v, want a loud missing-vendor-id failure", err)
	}
}

func TestRePullFailureSurfacesToTheCaller(t *testing.T) {
	// Arrange
	history := &fakeHistory{err: errors.New("store down")}
	h := newRepullHarness(t, history, "vendor-uuid")
	h.seq.SetLastSeq("s1", 9)
	// Act
	err := h.m.Resync("ws", 0)
	// Assert
	if err == nil || !strings.Contains(err.Error(), "store down") {
		t.Fatalf("err = %v, want the store failure surfaced", err)
	}
}

func TestRePullClearsItsInFlightMarkOnFailure(t *testing.T) {
	// Arrange — a failed pull that stayed "in flight" would wedge the workspace.
	history := &fakeHistory{err: errors.New("store down")}
	h := newRepullHarness(t, history, "vendor-uuid")
	h.seq.SetLastSeq("s1", 9)
	_ = h.m.Resync("ws", 0)
	// Act
	_ = h.m.Resync("ws", 0)
	// Assert
	if history.callCount() != 2 {
		t.Fatalf("re-pulled %d time(s), want 2 (the first failure released the mark)", history.callCount())
	}
}
