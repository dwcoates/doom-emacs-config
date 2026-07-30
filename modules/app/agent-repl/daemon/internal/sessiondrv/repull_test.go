package sessiondrv

import (
	"context"
	"errors"
	"strings"
	"sync"
	"testing"
	"time"

	corev1 "agentrepl/proto/agentshim/core/v1"
	datav1 "agentrepl/proto/agentshim/data/v1"

	"claude-repld/internal/shimclient"

	"google.golang.org/protobuf/types/known/anypb"
)

// --- fakes ------------------------------------------------------------------

// replayClient is a fakeClient whose Replay is scripted: it records what the
// driver asked the SHIM for and streams a canned range back.
type replayClient struct {
	fakeClient

	mu sync.Mutex
	// calls records one entry per Replay, as [from, to].
	calls [][2]uint64
	// caps records the max_events each request carried.
	caps   []uint32
	events []*corev1.Event
	result shimclient.ReplayResult
	err    error
	// queuedErrs are per-call errors, popped one per Replay before `err` takes
	// over. It is how a test scripts a replay that fails ONCE and succeeds on
	// the retry — the shape a shim-link bounce leaves behind.
	queuedErrs []error
	// block, when non-nil, holds Replay open until it is closed — the
	// in-flight shape the concurrency guard is about.
	block chan struct{}
}

type manualRepullTimer struct {
	mu       sync.Mutex
	callback func()
	resets   []time.Duration
	stopped  bool
}

func (t *manualRepullTimer) Stop() bool {
	t.mu.Lock()
	defer t.mu.Unlock()
	wasActive := !t.stopped
	t.stopped = true
	return wasActive
}

func (t *manualRepullTimer) Reset(d time.Duration) bool {
	t.mu.Lock()
	defer t.mu.Unlock()
	wasActive := !t.stopped
	t.stopped = false
	t.resets = append(t.resets, d)
	return wasActive
}

func (t *manualRepullTimer) fire() {
	t.mu.Lock()
	callback := t.callback
	stopped := t.stopped
	t.mu.Unlock()
	if !stopped {
		callback()
	}
}

func (c *replayClient) Replay(_ context.Context, from, to uint64, maxEvents uint32, onEvent func(*corev1.Event)) (shimclient.ReplayResult, error) {
	c.mu.Lock()
	c.calls = append(c.calls, [2]uint64{from, to})
	c.caps = append(c.caps, maxEvents)
	block := c.block
	events := c.events
	result := c.result
	err := c.err
	if len(c.queuedErrs) > 0 {
		err = c.queuedErrs[0]
		c.queuedErrs = c.queuedErrs[1:]
	}
	c.mu.Unlock()
	if block != nil {
		<-block
	}
	for _, ev := range events {
		onEvent(ev)
	}
	return result, err
}

func (c *replayClient) callCount() int {
	c.mu.Lock()
	defer c.mu.Unlock()
	return len(c.calls)
}

func TestRepullActivityKeepsALargeSlowReplayAliveOnProgress(t *testing.T) {
	// Arrange: deterministic logical time models 3,702 events arriving nine
	// seconds apart. The replay lasts more than nine hours in logical time,
	// vastly longer than its ten-second idle bound, without one idle interval.
	const eventCount = 3702
	const idle = 10 * time.Second
	now := time.Unix(0, 0)
	var timer *manualRepullTimer
	activity := newRepullActivityWithClock(context.Background(), idle, func() time.Time {
		return now
	}, func(_ time.Duration, callback func()) repullTimer {
		timer = &manualRepullTimer{callback: callback}
		return timer
	})
	defer activity.stop()

	// Act: force the timer callback after each progress update too. expire must
	// observe the fresh timestamp and rearm instead of cancelling from a stale
	// callback that raced with progress.
	for seq := uint64(1); seq <= eventCount; seq++ {
		now = now.Add(9 * time.Second)
		activity.observe(seq)
		timer.fire()
		select {
		case <-activity.ctx.Done():
			t.Fatalf("activity context canceled at seq=%d after progress: %v", seq, context.Cause(activity.ctx))
		default:
		}
	}

	// Assert
	snapshot := activity.stop()
	if snapshot.delivered != eventCount || snapshot.firstSeq != 1 || snapshot.lastSeq != eventCount {
		t.Fatalf("snapshot=%+v, want %d events spanning [1,%d]", snapshot, eventCount, eventCount)
	}
	if snapshot.elapsed <= idle {
		t.Fatalf("logical replay elapsed=%s, want longer than idle bound=%s", snapshot.elapsed, idle)
	}
}

func TestRepullActivityCancelsAfterOneIdleWindowWithoutProgress(t *testing.T) {
	// Arrange
	const idle = 10 * time.Second
	now := time.Unix(0, 0)
	var timer *manualRepullTimer
	activity := newRepullActivityWithClock(context.Background(), idle, func() time.Time {
		return now
	}, func(_ time.Duration, callback func()) repullTimer {
		timer = &manualRepullTimer{callback: callback}
		return timer
	})
	defer activity.stop()

	// Act: no event arrives before the finite no-progress bound.
	now = now.Add(idle)
	timer.fire()

	// Assert
	select {
	case <-activity.ctx.Done():
	default:
		t.Fatal("idle replay remained live after its no-progress bound")
	}
	var idleErr *repullNoProgressError
	if cause := context.Cause(activity.ctx); !errors.As(cause, &idleErr) {
		t.Fatalf("cancellation cause=%v, want *repullNoProgressError", cause)
	}
	if idleErr.delivered != 0 || idleErr.firstSeq != 0 || idleErr.lastSeq != 0 {
		t.Fatalf("idle cause=%+v, want zero-progress accounting", idleErr)
	}
}

// repullHarness is one live driver whose collaborators are all recorders, so a
// replay's blast radius is directly assertable.
type repullHarness struct {
	m        *Manager
	push     *fakePusher
	applier  *fakeApplier
	progress *fakeProgress
	client   *replayClient
	seq      *fakeSeqStore
	floors   *fakeClearCompactStore
	// reg is the rotation-aware registrar (rotationpurge_test.go): it mirrors
	// the production adapter by zeroing both cursors in the same act that
	// adopts a new vendor uuid, so a rotation test cannot pass against a fake
	// that forgot the reset.
	reg *rotatingRegistrar
}

func newRepullHarness(t *testing.T, client *replayClient) *repullHarness {
	t.Helper()
	return newRepullHarnessWithLog(t, client, nil)
}

// newRepullHarnessWithLog is newRepullHarness with the daemon log captured, for
// tests asserting on a loud line. logf may be nil (the plain harness).
func newRepullHarnessWithLog(t *testing.T, client *replayClient, logf func(string, ...any)) *repullHarness {
	t.Helper()
	h := &repullHarness{
		push:     &fakePusher{},
		applier:  &fakeApplier{},
		progress: &fakeProgress{},
		client:   client,
		seq:      &fakeSeqStore{seq: map[string]uint64{}},
		floors:   newFakeClearCompactStore(),
	}
	h.reg = &rotatingRegistrar{seq: h.seq, floors: h.floors}
	m, err := New(Config{
		Push:              h.push,
		SSM:               h.applier,
		Progress:          h.progress,
		Spawner:           &fakeSpawner{},
		Locator:           fakeLocator{m: map[string]string{"ws": "s1"}},
		SeqStore:          h.seq,
		ClearCompactStore: h.floors,
		Registrar:         h.reg,
		ProtocolVersion:   "1",
		Logf:              logf,
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
	client := &replayClient{}
	h := newRepullHarness(t, client)
	h.driver(t).consumer.Consume(assistantEvent(t, 10, "u10"))
	// Act
	if err := h.m.Resync("ws", 10); err != nil {
		t.Fatalf("Resync: %v", err)
	}
	// Assert
	if client.callCount() != 0 {
		t.Fatalf("replayed %d time(s) for an in-window resync, want 0", client.callCount())
	}
}

func TestResyncBelowTheRingFloorAsksTheShimForTheGap(t *testing.T) {
	// Arrange — the ring's floor is seq 10; the frontend asks from 0.
	client := &replayClient{}
	h := newRepullHarness(t, client)
	h.driver(t).consumer.Consume(assistantEvent(t, 10, "u10"))
	// Act
	if err := h.m.Resync("ws", 0); err != nil {
		t.Fatalf("Resync: %v", err)
	}
	// Assert
	client.mu.Lock()
	defer client.mu.Unlock()
	if len(client.calls) != 1 || client.calls[0] != [2]uint64{0, 10} {
		t.Fatalf("replay calls = %v, want one [0 10]", client.calls)
	}
}

func TestRePullCarriesTheRequestersEventCap(t *testing.T) {
	// Arrange — the bound is the REQUESTER's stated policy, not something the
	// shim invents on the daemon's behalf.
	client := &replayClient{}
	h := newRepullHarness(t, client)
	h.seq.SetLastSeq("s1", 9)
	// Act
	if err := h.m.Resync("ws", 0); err != nil {
		t.Fatalf("Resync: %v", err)
	}
	// Assert
	client.mu.Lock()
	defer client.mu.Unlock()
	if len(client.caps) != 1 || client.caps[0] != repullMaxEvents {
		t.Fatalf("replay caps = %v, want [%d]", client.caps, repullMaxEvents)
	}
}

func TestEmptyRingTakesItsFloorFromTheDurableHighWaterMark(t *testing.T) {
	// Arrange — a restarted daemon: nothing retained, but last_seen_seq survives.
	client := &replayClient{}
	h := newRepullHarness(t, client)
	h.seq.SetLastSeq("s1", 7117)
	// Act
	if err := h.m.Resync("ws", 0); err != nil {
		t.Fatalf("Resync: %v", err)
	}
	// Assert
	client.mu.Lock()
	defer client.mu.Unlock()
	if len(client.calls) != 1 || client.calls[0] != [2]uint64{0, 7118} {
		t.Fatalf("replay calls = %v, want one [0 7118]", client.calls)
	}
}

func TestReplayedEventsReachConversation(t *testing.T) {
	// Arrange
	client := &replayClient{events: []*corev1.Event{assistantEvent(t, 3, "old")}}
	h := newRepullHarness(t, client)
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

func TestReplayedEventsNeverReachTheSSM(t *testing.T) {
	// Arrange — the SSM consumed this history once already; re-applying it is
	// what drives live_task_count into impossible values.
	client := &replayClient{events: []*corev1.Event{
		{SessionId: "vendor-uuid", Seq: 3, Payload: &corev1.Event_TaskEnded{TaskEnded: &corev1.TaskEnded{TaskId: "t1"}}},
		assistantEvent(t, 4, "old"),
	}}
	h := newRepullHarness(t, client)
	h.seq.SetLastSeq("s1", 9)
	// Act
	if err := h.m.Resync("ws", 0); err != nil {
		t.Fatalf("Resync: %v", err)
	}
	// Assert
	if len(h.applier.applied) != 0 {
		t.Fatalf("SSM saw %d replayed event(s), want 0", len(h.applier.applied))
	}
}

func TestReplayedEventsNeverReachTheProgressResolver(t *testing.T) {
	// Arrange
	client := &replayClient{events: []*corev1.Event{assistantEvent(t, 3, "old")}}
	h := newRepullHarness(t, client)
	h.seq.SetLastSeq("s1", 9)
	// Act
	if err := h.m.Resync("ws", 0); err != nil {
		t.Fatalf("Resync: %v", err)
	}
	// Assert
	if len(h.progress.applied) != 0 {
		t.Fatalf("progress saw %d replayed event(s), want 0", len(h.progress.applied))
	}
}

func TestReplayedEventsNeverRebuildTheTaskCatalog(t *testing.T) {
	// Arrange — a historical task lifecycle must not repopulate the roster.
	client := &replayClient{events: []*corev1.Event{
		{SessionId: "vendor-uuid", Seq: 3, Payload: &corev1.Event_TaskStarted{TaskStarted: &corev1.TaskStarted{TaskId: "t1"}}},
	}}
	h := newRepullHarness(t, client)
	h.seq.SetLastSeq("s1", 9)
	// Act
	if err := h.m.Resync("ws", 0); err != nil {
		t.Fatalf("Resync: %v", err)
	}
	// Assert
	h.push.mu.Lock()
	defer h.push.mu.Unlock()
	if len(h.push.catalog) != 0 {
		t.Fatalf("replay pushed %d task catalog(s), want 0", len(h.push.catalog))
	}
}

func TestReplayedEventsNeverEnterTheRetainedRing(t *testing.T) {
	// Arrange — back-filling the live window would drift the floor under the
	// next request.
	client := &replayClient{events: []*corev1.Event{assistantEvent(t, 3, "old")}}
	h := newRepullHarness(t, client)
	h.seq.SetLastSeq("s1", 9)
	// Act
	if err := h.m.Resync("ws", 0); err != nil {
		t.Fatalf("Resync: %v", err)
	}
	// Assert
	if got := len(h.driver(t).consumer.snapshotRing()); got != 0 {
		t.Fatalf("ring holds %d replayed event(s), want 0", got)
	}
}

func TestConcurrentCoveredRePullCoalesces(t *testing.T) {
	// Arrange — a replay from 0 is already running when a second asks from 5.
	client := &replayClient{block: make(chan struct{})}
	h := newRepullHarness(t, client)
	h.seq.SetLastSeq("s1", 100)
	started := make(chan struct{})
	go func() {
		close(started)
		_ = h.m.Resync("ws", 0)
	}()
	<-started
	waitFor(t, "the first replay to start", func() bool { return client.callCount() == 1 })
	// Act
	err := h.m.Resync("ws", 5)
	close(client.block)
	// Assert
	if err != nil {
		t.Fatalf("a covered concurrent resync must coalesce, got %v", err)
	}
}

func TestConcurrentUncoveredRePullIsRefusedLoudly(t *testing.T) {
	// Arrange — a replay from 50 is running when a second asks from 5, which it
	// does NOT cover.
	client := &replayClient{block: make(chan struct{})}
	h := newRepullHarness(t, client)
	h.seq.SetLastSeq("s1", 100)
	started := make(chan struct{})
	go func() {
		close(started)
		_ = h.m.Resync("ws", 50)
	}()
	<-started
	waitFor(t, "the first replay to start", func() bool { return client.callCount() == 1 })
	// Act
	err := h.m.Resync("ws", 5)
	close(client.block)
	// Assert
	if !errors.Is(err, ErrRepullInFlight) {
		t.Fatalf("err = %v, want ErrRepullInFlight", err)
	}
}

func TestTruncatedReplayIsReportedNotPassedOffAsComplete(t *testing.T) {
	// Arrange — the shim hit a bound before reaching the floor.
	client := &replayClient{result: shimclient.ReplayResult{Delivered: 12, Truncated: true, Reason: "hit the cap"}}
	h := newRepullHarness(t, client)
	h.seq.SetLastSeq("s1", 9)
	// Act
	err := h.m.Resync("ws", 0)
	// Assert
	if !errors.Is(err, ErrRepullTruncated) {
		t.Fatalf("err = %v, want ErrRepullTruncated", err)
	}
}

func TestTruncatedReplayNamesTheShimsReason(t *testing.T) {
	// Arrange
	client := &replayClient{result: shimclient.ReplayResult{Truncated: true, Reason: "store subscription idle"}}
	logs := &logCapture{}
	h := newRepullHarnessWithLog(t, client, logs.logf)
	h.seq.SetLastSeq("s1", 9)
	// Act
	err := h.m.Resync("ws", 0)
	// Assert
	if err == nil || !strings.Contains(err.Error(), "store subscription idle") {
		t.Fatalf("err = %v, want the shim's own reason carried through", err)
	}
	if !logs.contains(`history re-pull TRUNCATED ws="ws" session=s1`) ||
		!logs.contains(`reason="store subscription idle"`) {
		t.Fatal("truncation log lacks workspace, session, and owned cause")
	}
}

func TestReplayFailureSurfacesToTheCaller(t *testing.T) {
	// Arrange
	client := &replayClient{
		events: []*corev1.Event{assistantEvent(t, 3, "old")},
		err:    errors.New("shim went away"),
	}
	logs := &logCapture{}
	h := newRepullHarnessWithLog(t, client, logs.logf)
	h.seq.SetLastSeq("s1", 9)
	// Act
	err := h.m.Resync("ws", 0)
	// Assert
	if err == nil || !strings.Contains(err.Error(), "shim went away") {
		t.Fatalf("err = %v, want the shim failure surfaced", err)
	}
	if !logs.contains(`history re-pull FAILED ws="ws" session=s1`) ||
		!logs.contains(`delivered=1 first_seq=3 last_seq=3`) ||
		!logs.contains(`cause="shim went away"`) {
		t.Fatal("replay failure log lacks progress range and owned cause")
	}
}

func TestRePullClearsItsInFlightMarkOnFailure(t *testing.T) {
	// Arrange — a failed replay that stayed "in flight" would wedge the workspace.
	client := &replayClient{err: errors.New("shim went away")}
	h := newRepullHarness(t, client)
	h.seq.SetLastSeq("s1", 9)
	_ = h.m.Resync("ws", 0)
	// Act
	_ = h.m.Resync("ws", 0)
	// Assert
	if client.callCount() != 2 {
		t.Fatalf("replayed %d time(s), want 2 (the first failure released the mark)", client.callCount())
	}
}
