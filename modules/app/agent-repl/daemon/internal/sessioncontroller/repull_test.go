package sessioncontroller

import (
	"context"
	"errors"
	"fmt"
	"strings"
	"sync"
	"testing"
	"time"

	corev1 "agentrepl/proto/agentshim/core/v1"
	datav1 "agentrepl/proto/agentshim/data/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/shimclient"

	"google.golang.org/protobuf/proto"
	"google.golang.org/protobuf/types/known/anypb"
)

// --- fakes ------------------------------------------------------------------

// replayClient is a fakeClient whose Replay is scripted: it records what the
// controller asked the SHIM for and streams a canned range back.
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
	// entered, when non-nil, is closed the first time Replay is ENTERED, so a
	// concurrency test hands off on a channel instead of polling for a call
	// count. Closed before the block is waited on, which is the instant the
	// workspace's re-pull slot is provably held.
	entered     chan struct{}
	enteredOnce sync.Once
}

// newRepullWaitWatcher is a log capture that also SIGNALS the moment a request
// announces it is waiting behind an in-flight re-pull, which is the handoff a
// serialization test needs and the loud line the operator needs.
type repullWaitWatcher struct {
	logCapture
	waiting chan struct{}
	once    sync.Once
}

func newRepullWaitWatcher() *repullWaitWatcher {
	return &repullWaitWatcher{waiting: make(chan struct{})}
}

func (w *repullWaitWatcher) logf(format string, args ...any) {
	w.logCapture.logf(format, args...)
	if strings.Contains(fmt.Sprintf(format, args...), "WAITING for the in-flight re-pull") {
		w.once.Do(func() { close(w.waiting) })
	}
}

// inFlightRepull returns the workspace's in-flight re-pull state, read under the
// lock that guards it.
func (h *repullHarness) inFlightRepull(t *testing.T) *repullState {
	t.Helper()
	d := h.controller(t)
	h.m.mu.Lock()
	defer h.m.mu.Unlock()
	if d.repull == nil {
		t.Fatal("no re-pull is in flight")
	}
	return d.repull
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
	notifyTestActivity()
	if c.entered != nil {
		c.enteredOnce.Do(func() { close(c.entered) })
	}
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

// repullHarness is one live session controller whose collaborators are all recorders, so a
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
	// history is the DurableHistorySource the gap's store-covered prefix is
	// read from. Nil in the plain harness, which is the "no durable source is
	// wired" shape: the whole gap then goes to the shim.
	history DurableHistorySource
}

func newRepullHarness(t *testing.T, client *replayClient) *repullHarness {
	t.Helper()
	return newRepullHarnessWithLog(t, client, nil)
}

// newRepullHarnessWithLog is newRepullHarness with the daemon log captured, for
// tests asserting on a loud line. logf may be nil (the plain harness).
func newRepullHarnessWithLog(t *testing.T, client *replayClient, logf func(string, ...any)) *repullHarness {
	t.Helper()
	return newRepullHarnessWithStore(t, client, nil, logf)
}

// newRepullHarnessWithStore is newRepullHarnessWithLog with a
// DurableHistorySource wired, which is the production shape: the gap's
// store-covered prefix is served from durable history and only the remainder
// above the high-water mark is asked of the shim.
func newRepullHarnessWithStore(t *testing.T, client *replayClient, history DurableHistorySource, logf func(string, ...any)) *repullHarness {
	t.Helper()
	h := &repullHarness{
		push:     &fakePusher{},
		applier:  &fakeApplier{},
		progress: &fakeProgress{},
		client:   client,
		seq:      &fakeSeqStore{seq: map[string]uint64{}},
		floors:   newFakeClearCompactStore(),
		history:  history,
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
		TurnAccountings:   emptyTurnAccountingStore{},
		DurableHistory:    history,
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

// controller returns the live session controller for "ws".
func (h *repullHarness) controller(t *testing.T) *sessionController {
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
	h.controller(t).consumer.Consume(assistantEvent(t, 10, "u10"))
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
	h.controller(t).consumer.Consume(assistantEvent(t, 10, "u10"))
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

func TestConnectedRepullAttachesHistoricalAccountingByPersistedTurnIdentityWhileIdle(t *testing.T) {
	// Arrange: the connected consumer is idle, so no live reducer turn can
	// accidentally provide the identity for historical turn T1.
	result := accountingVendorEvent(t, &datav1.ClaudeStreamMessage{Msg: &datav1.ClaudeStreamMessage_Result{Result: &datav1.ResultMessage{}}})
	result.Seq = 3
	result.RequestId = "T1"
	client := &replayClient{events: []*corev1.Event{result}}
	h := newRepullHarness(t, client)
	want := &frontendv1.TurnAccounting{TurnId: "T1", QueryInstanceId: "query-1", Verdict: &frontendv1.TurnAccounting_Complete{Complete: &frontendv1.TurnAccountingComplete{}}}
	h.m.cfg.TurnAccountings = replayTurnAccountingStore{accountings: []*frontendv1.TurnAccounting{want}}
	h.seq.SetLastSeq("s1", 9)
	if active := h.controller(t).consumer.accounting.activeTurnID; active != "" {
		t.Fatalf("active turn = %q, want idle", active)
	}

	// Act.
	if err := h.m.Resync("ws", 0); err != nil {
		t.Fatalf("Resync: %v", err)
	}

	// Assert: the result's durable request/turn identity, not mutable reducer
	// state, selects the exact persisted record.
	h.push.mu.Lock()
	defer h.push.mu.Unlock()
	if len(h.push.convo) != 1 || len(h.push.convo[0].GetItems()) != 1 || !proto.Equal(h.push.convo[0].GetItems()[0].GetTurnAccounting(), want) {
		t.Fatalf("conversation pushes = %+v", h.push.convo)
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
	if got := len(h.controller(t).consumer.snapshotRing()); got != 0 {
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

func TestConcurrentUncoveredRePullWaitsThenServesItsOwnRange(t *testing.T) {
	// Arrange — a replay from 50 is running when a second asks from 5, which it
	// does NOT cover. The second must be SERVED, not nacked: no caller retries a
	// refusal, so a refusal is a permanently missing stretch of history.
	client := &replayClient{block: make(chan struct{}), entered: make(chan struct{})}
	w := newRepullWaitWatcher()
	h := newRepullHarnessWithLog(t, client, w.logf)
	h.seq.SetLastSeq("s1", 100)
	go func() { _ = h.m.Resync("ws", 50) }()
	<-client.entered
	// Act — the second request blocks in the wait, and the first is released
	// only once it is provably waiting.
	second := make(chan error, 1)
	go func() { second <- h.m.Resync("ws", 5) }()
	<-w.waiting
	close(client.block)
	err := <-second
	// Assert
	if err != nil {
		t.Fatalf("an uncovered concurrent resync must wait and then run, got %v", err)
	}
	client.mu.Lock()
	defer client.mu.Unlock()
	if len(client.calls) != 2 || client.calls[1] != [2]uint64{4, 101} {
		t.Fatalf("replay calls = %v, want the waiter's own FULL range [4 101] as the second call", client.calls)
	}
}

func TestConcurrentUncoveredRePullSaysItIsWaiting(t *testing.T) {
	// Arrange — the serialization must be readable in the log, since from the
	// outside a waiting request is indistinguishable from a slow one.
	client := &replayClient{block: make(chan struct{}), entered: make(chan struct{})}
	w := newRepullWaitWatcher()
	h := newRepullHarnessWithLog(t, client, w.logf)
	h.seq.SetLastSeq("s1", 100)
	go func() { _ = h.m.Resync("ws", 50) }()
	<-client.entered
	// Act
	second := make(chan error, 1)
	go func() { second <- h.m.Resync("ws", 5) }()
	<-w.waiting
	close(client.block)
	<-second
	// Assert
	// The announced mark is the pull's own exclusive lower bound (the client's
	// from_seq=5 minus one), which is the number the served range is expressed in.
	if !w.contains(`WAITING for the in-flight re-pull, then serving from_seq=4`) {
		t.Fatal("the wait is not announced in the log")
	}
}

func TestUncoveredRePullWaitFailsLoudlyWhenTheDaemonShutsDown(t *testing.T) {
	// Arrange — a waiter must not outlive the daemon that would serve it.
	client := &replayClient{block: make(chan struct{}), entered: make(chan struct{})}
	w := newRepullWaitWatcher()
	h := newRepullHarnessWithLog(t, client, w.logf)
	h.seq.SetLastSeq("s1", 100)
	go func() { _ = h.m.Resync("ws", 50) }()
	<-client.entered
	second := make(chan error, 1)
	go func() { second <- h.m.Resync("ws", 5) }()
	<-w.waiting
	// Act
	h.m.rootStop()
	err := <-second
	close(client.block)
	// Assert
	if !errors.Is(err, ErrRepullInFlight) {
		t.Fatalf("err = %v, want ErrRepullInFlight naming the abandoned wait", err)
	}
}

func TestUncoveredRePullWaitFailsLoudlyWhenTheInFlightPullOverrunsItsGrace(t *testing.T) {
	// Arrange — the in-flight pull's own deadline trips while it is wedged in
	// the shim, so it never releases the workspace. The waiter owes the client
	// an answer rather than an unbounded wait.
	client := &replayClient{block: make(chan struct{}), entered: make(chan struct{})}
	w := newRepullWaitWatcher()
	h := newRepullHarnessWithLog(t, client, w.logf)
	h.m.repullWaitGraceOverride = time.Millisecond
	h.seq.SetLastSeq("s1", 100)
	go func() { _ = h.m.Resync("ws", 50) }()
	<-client.entered
	second := make(chan error, 1)
	go func() { second <- h.m.Resync("ws", 5) }()
	<-w.waiting
	// Act — trip the in-flight pull's deadline without letting it return.
	h.inFlightRepull(t).activity.cancel(errors.New("test: the in-flight pull's own deadline"))
	err := <-second
	close(client.block)
	// Assert
	if !errors.Is(err, ErrRepullInFlight) {
		t.Fatalf("err = %v, want ErrRepullInFlight for a wedged in-flight re-pull", err)
	}
}

func TestUncoveredRePullReEvaluatesTheEpochAfterTheWait(t *testing.T) {
	// Arrange — the waiter's bounds were computed before the wait, so a rotation
	// during the wait retires the seq space they count in. Serving them against
	// the new space is the ceiling-from-a-retired-space defect.
	client := &replayClient{block: make(chan struct{}), entered: make(chan struct{})}
	w := newRepullWaitWatcher()
	h := newRepullHarnessWithLog(t, client, w.logf)
	h.seq.SetLastSeq("s1", 100)
	go func() { _ = h.m.Resync("ws", 50) }()
	<-client.entered
	second := make(chan error, 1)
	go func() { second <- h.m.Resync("ws", 5) }()
	<-w.waiting
	// Act
	h.rotate("uuid-old", "uuid-new")
	close(client.block)
	err := <-second
	// Assert
	if !errors.Is(err, ErrRepullInFlight) {
		t.Fatalf("err = %v, want ErrRepullInFlight for bounds retired by a rotation during the wait", err)
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

// --- the gap is split at the durable high-water mark -------------------------
//
// A below-floor gap is history that was already ingested, so the store the shim
// itself reads normally holds every row of it. Asking the shim for a range the
// store demonstrably covers made a frontend resync require a live shim it did
// not need, and a session with no shim answered the whole gap with
// ErrReplayNotConnected while every row sat in the store.

func TestGapSplitsAtTheStoreHighWater(t *testing.T) {
	tests := []struct {
		name string
		// ringSeq, when non-zero, is the one event retained, which puts the
		// ring floor there. Zero leaves the ring empty (a restarted daemon).
		ringSeq uint64
		// highWater is the durable last_seen_seq: how far the store's seq
		// space is known to reach for this session.
		highWater uint64
		// wireStore is false for the "no DurableHistorySource" shape.
		wireStore  bool
		resyncFrom uint64
		wantStore  [][2]uint64
		wantShim   [][2]uint64
	}{
		{
			name:       "fully covered by the store, so no shim is involved",
			highWater:  1172,
			wireStore:  true,
			resyncFrom: 1136,
			wantStore:  [][2]uint64{{1135, 1173}},
			wantShim:   nil,
		},
		{
			name:       "partially above the high-water, so the shim serves the remainder only",
			ringSeq:    20,
			highWater:  9,
			wireStore:  true,
			resyncFrom: 0,
			wantStore:  [][2]uint64{{0, 10}},
			wantShim:   [][2]uint64{{9, 20}},
		},
		{
			name:       "entirely above the high-water, so the whole gap is the shim's",
			ringSeq:    20,
			highWater:  0,
			wireStore:  true,
			resyncFrom: 5,
			wantStore:  nil,
			wantShim:   [][2]uint64{{4, 20}},
		},
		{
			name:       "no durable source wired, so the whole gap is the shim's",
			ringSeq:    20,
			highWater:  9,
			wireStore:  false,
			resyncFrom: 0,
			wantStore:  nil,
			wantShim:   [][2]uint64{{0, 20}},
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			// Arrange
			client := &replayClient{}
			store := &durableHistorySpy{}
			var history DurableHistorySource
			if tc.wireStore {
				history = store
			}
			h := newRepullHarnessWithStore(t, client, history, nil)
			if tc.ringSeq != 0 {
				h.controller(t).consumer.Consume(assistantEvent(t, tc.ringSeq, "ring"))
			}
			h.seq.SetLastSeq("s1", tc.highWater)

			// Act
			if err := h.m.Resync("ws", tc.resyncFrom); err != nil {
				t.Fatalf("Resync: %v", err)
			}

			// Assert
			if got := store.replays(); !equalReplayRanges(got, tc.wantStore) {
				t.Fatalf("store replays = %v, want %v", got, tc.wantStore)
			}
			client.mu.Lock()
			defer client.mu.Unlock()
			if !equalReplayRanges(client.calls, tc.wantShim) {
				t.Fatalf("shim replays = %v, want %v", client.calls, tc.wantShim)
			}
		})
	}
}

// equalReplayRanges compares two replay-bound lists, treating nil and empty alike.
func equalReplayRanges(got, want [][2]uint64) bool {
	if len(got) != len(want) {
		return false
	}
	for i := range got {
		if got[i] != want[i] {
			return false
		}
	}
	return true
}

func TestStoreCoveredGapIsServedWithNoLiveShim(t *testing.T) {
	// Arrange — the session has NO live shim connection, and the whole gap is
	// at or below the store's high-water mark.
	client := &replayClient{err: shimclient.ErrReplayNotConnected}
	store := &durableHistorySpy{events: []*corev1.Event{assistantEvent(t, 1136, "stored")}}
	h := newRepullHarnessWithStore(t, client, store, nil)
	h.seq.SetLastSeq("s1", 1172)

	// Act
	err := h.m.Resync("ws", 1136)

	// Assert
	if err != nil {
		t.Fatalf("Resync: %v, want the store to have served the gap without a shim", err)
	}
	h.push.mu.Lock()
	defer h.push.mu.Unlock()
	if len(h.push.convo) != 1 || h.push.convo[0].GetItems()[0].GetUuid() != "stored" {
		t.Fatalf("conversation pushes = %v, want one item uuid=stored", h.push.convo)
	}
}

func TestStoreServedGapNamesTheStoreAsItsSource(t *testing.T) {
	// Arrange — which route served which range must be readable from the log.
	client := &replayClient{}
	store := &durableHistorySpy{events: []*corev1.Event{assistantEvent(t, 1136, "stored")}}
	logs := &logCapture{}
	h := newRepullHarnessWithStore(t, client, store, logs.logf)
	h.seq.SetLastSeq("s1", 1172)

	// Act
	if err := h.m.Resync("ws", 1136); err != nil {
		t.Fatalf("Resync: %v", err)
	}

	// Assert
	if !logs.contains(`fully covered by the store high-water 1172`) ||
		!logs.contains(`re-pull segment COMPLETE ws="ws" session=s1 source=store from_seq=1135 stop_at=1173 delivered=1`) {
		t.Fatal("store-served gap log names neither the source nor the range it covered")
	}
}

func TestSplitGapLogsTheRangeEachSourceCovers(t *testing.T) {
	// Arrange — a gap straddling the high-water mark.
	client := &replayClient{}
	store := &durableHistorySpy{}
	logs := &logCapture{}
	h := newRepullHarnessWithStore(t, client, store, logs.logf)
	h.controller(t).consumer.Consume(assistantEvent(t, 20, "ring"))
	h.seq.SetLastSeq("s1", 9)

	// Act
	if err := h.m.Resync("ws", 0); err != nil {
		t.Fatalf("Resync: %v", err)
	}

	// Assert
	if !logs.contains(`SPLIT at the store high-water 9: store serves (0,10), the shim serves the remainder (9,20)`) {
		t.Fatal("split log does not state which source covers which range")
	}
}

func TestRemainderAboveTheHighWaterWithNoShimFailsNamingTheUncoveredRange(t *testing.T) {
	// Arrange — the store covers the gap's prefix; the remainder above its
	// high-water has no shim to serve it, which stays a loud failure.
	client := &replayClient{err: shimclient.ErrReplayNotConnected}
	store := &durableHistorySpy{}
	logs := &logCapture{}
	h := newRepullHarnessWithStore(t, client, store, logs.logf)
	h.controller(t).consumer.Consume(assistantEvent(t, 20, "ring"))
	h.seq.SetLastSeq("s1", 9)

	// Act
	err := h.m.Resync("ws", 0)

	// Assert
	if !errors.Is(err, shimclient.ErrReplayNotConnected) {
		t.Fatalf("err = %v, want ErrReplayNotConnected for the uncovered remainder", err)
	}
	if !strings.Contains(err.Error(), "from_seq=9 stop_at=20") {
		t.Fatalf("err = %v, want the uncovered remainder named", err)
	}
	if !logs.contains(`history re-pull FAILED ws="ws" session=s1 source=shim from_seq=9 stop_at=20`) {
		t.Fatal("remainder failure log names neither the source nor the uncovered range")
	}
}

func TestStoreReadFailureIsSurfacedAndTheShimIsNotAskedInstead(t *testing.T) {
	// Arrange — a broken store must not be answered by re-asking the shim for
	// the same range, which is the fallback this path refuses to be.
	client := &replayClient{}
	store := &durableHistorySpy{err: errors.New("store socket refused")}
	logs := &logCapture{}
	h := newRepullHarnessWithStore(t, client, store, logs.logf)
	h.seq.SetLastSeq("s1", 1172)

	// Act
	err := h.m.Resync("ws", 1136)

	// Assert
	if err == nil || !strings.Contains(err.Error(), "store socket refused") {
		t.Fatalf("err = %v, want the store failure surfaced", err)
	}
	if client.callCount() != 0 {
		t.Fatalf("shim replayed %d time(s) after a store read failure, want 0", client.callCount())
	}
	if !logs.contains(`history re-pull FAILED ws="ws" session=s1 source=store from_seq=1135 stop_at=1173`) {
		t.Fatal("store failure log does not name the store as the source that failed")
	}
}

func TestTruncatedStoreReadIsReportedNotPassedOffAsComplete(t *testing.T) {
	// Arrange — the store hit a bound before reaching the ring floor.
	client := &replayClient{}
	store := &durableHistorySpy{truncated: "event cap 20000 reached"}
	h := newRepullHarnessWithStore(t, client, store, nil)
	h.seq.SetLastSeq("s1", 1172)

	// Act
	err := h.m.Resync("ws", 1136)

	// Assert
	if !errors.Is(err, ErrRepullTruncated) {
		t.Fatalf("err = %v, want ErrRepullTruncated", err)
	}
}
