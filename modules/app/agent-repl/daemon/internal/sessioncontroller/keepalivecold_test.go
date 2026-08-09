package sessioncontroller

import (
	"context"
	"errors"
	"runtime"
	"testing"
	"time"

	corev1 "agentrepl/proto/agentshim/core/v1"
	datav1 "agentrepl/proto/agentshim/data/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"

	"claude-repld/internal/keepalive"
	"claude-repld/internal/registry"
	"claude-repld/internal/shimclient"

	"google.golang.org/protobuf/types/known/anypb"
)

// coldPingCacheTTL and coldPingThreshold are the policy this file's rig runs.
// They are stated explicitly rather than taken from the shipped defaults so the
// assertions on the persisted account name figures the test itself chose.
const (
	coldPingCacheTTL          = time.Hour
	coldPingThreshold   int64 = 20000
	coldPingElapsedMs   int64 = 59 * 60 * 1000
	coldPingLastTurnEnd int64 = 1_700_000_000_000
)

// coldPingRig is a settled, awake, brought-up session with a hibernation
// registrar, a keep-alive window ledger, a captured log and a FIXED clock.
//
// The registrar is wired into the Config BEFORE the bring-up rather than
// attached afterwards, so the consumer's turn-end and result-cost hooks are
// bound exactly as production binds them.
func coldPingRig(t *testing.T) (*Manager, *fakeApplier, *fakeHibernations, *fakeKeepAliveWindows, *logCapture) {
	t.Helper()
	applier := &fakeApplier{}
	hib := newFakeHibernations()
	windows := newFakeKeepAliveWindows()
	capture := &logCapture{}
	m, err := New(Config{
		Logf:              capture.logf,
		Push:              &fakePusher{},
		Progress:          &fakeProgress{},
		SSM:               applier,
		Spawner:           &fakeSpawner{},
		Locator:           fakeLocator{m: map[string]string{"ws": "s1"}},
		SeqStore:          &fakeSeqStore{seq: map[string]uint64{}},
		ClearCompactStore: newFakeClearCompactStore(),
		TurnAccountings:   emptyTurnAccountingStore{},
		Registrar:         &fakeRegistrar{},
		Hibernations:      hib,
		KeepAliveWindows:  windows,
		KeepAlive: keepalive.Config{
			CacheTTL:                coldPingCacheTTL,
			Leeway:                  keepalive.DefaultLeeway,
			IdleCutoff:              keepalive.DefaultIdleCutoff,
			UncachedCostAlertTokens: coldPingThreshold,
		},
		ProtocolVersion: "1",
		Now:             func() int64 { return coldPingLastTurnEnd + coldPingElapsedMs },
		Source:          stubSource{},
		FileDiagnostics: fakeFileDiagnosticPersister{},
		newClient:       func(c shimclient.Config) sessionClient { return &fakeClient{cfg: c} },
	})
	if err != nil {
		t.Fatalf("New: %v", err)
	}
	t.Cleanup(m.Close)
	// The instant every keep-alive measurement is taken from. Under the rig's
	// fixed clock this puts the session 59 minutes into a one-hour cache — the
	// window the sweeper pings in, and the window the observed defect fired in.
	hib.TurnEndObserved("s1", coldPingLastTurnEnd)
	if err := m.Ensure("ws"); err != nil {
		t.Fatalf("Ensure: %v", err)
	}
	waitForWirings(applier, 1)
	m.onConnected("ws", "s1", &corev1.ShimHello{})
	applier.setCurrent("ws", &frontendv1.WorkspaceState{State: frontendv1.RenderState_RENDER_STATE_READY})
	return m, applier, hib, windows, capture
}

// submitColdPingRig submits a ping, names its turn on the record so the
// boundary can match it, and returns the ping's turn id.
func submitPingUnderTurn(t *testing.T, m *Manager) string {
	t.Helper()
	turnID, err := m.SubmitKeepAlivePing(context.Background(), "ws")
	if err != nil {
		t.Fatalf("SubmitKeepAlivePing: %v", err)
	}
	m.mu.Lock()
	m.byWS["ws"].turn = turnRecord{phase: turnPhaseNamed, turnID: turnID}
	m.mu.Unlock()
	return turnID
}

// controllerFor reaches the workspace's live session controller.
func controllerFor(t *testing.T, m *Manager) *sessionController {
	t.Helper()
	m.mu.Lock()
	defer m.mu.Unlock()
	d, ok := m.byWS["ws"]
	if !ok {
		t.Fatal("the workspace has no live session controller")
	}
	return d
}

// awaitHibernation blocks on the registrar's write seam. It is the whole
// synchronization: the transition runs on its own goroutine, and a test that
// slept for it would be asserting on the scheduler.
func awaitHibernation(t *testing.T, hib *fakeHibernations) registry.HibernationDetail {
	t.Helper()
	select {
	case detail := <-hib.writeSeen:
		return detail
	case <-time.After(5 * time.Second):
		t.Fatal("no hibernation was recorded; the cold ping's proof went nowhere")
		return registry.HibernationDetail{}
	}
}

// ---------------------------------------------------------------------------
// The verdict
// ---------------------------------------------------------------------------

// A PING THAT PAID FOR THE WHOLE CONVERSATION IS PROOF THE CACHE WAS GONE, and
// the proof is what puts the session to sleep. Before this the same fact was
// detected, logged and rendered as a red footer line, and the session stayed
// awake with nothing offered to the user.
func TestColdKeepAlivePingHibernatesWithCacheExpired(t *testing.T) {
	// Arrange.
	m, _, hib, _, _ := coldPingRig(t)
	hib.writeSeen = make(chan registry.HibernationDetail, 4)
	turnID := submitPingUnderTurn(t, m)
	d := controllerFor(t, m)
	m.noteKeepAlivePingCost(d, costOf(turnID, uint64(coldPingThreshold+1), 0, 0))

	// Act.
	m.onTurnBoundary(d, false, coldPingLastTurnEnd+coldPingElapsedMs)

	// Assert.
	detail := awaitHibernation(t, hib)
	if detail.Cause != registry.HibernationCauseCacheExpired {
		t.Fatalf("hibernation cause = %q, want %q", detail.Cause, registry.HibernationCauseCacheExpired)
	}
}

// A PING THAT CAME BACK CHEAP PROVES THE OPPOSITE, and the opposite of a cold
// ping is the feature working. Nothing is stopped.
func TestWarmKeepAlivePingTakesNoHibernation(t *testing.T) {
	// Arrange.
	m, _, hib, _, _ := coldPingRig(t)
	turnID := submitPingUnderTurn(t, m)
	d := controllerFor(t, m)
	m.noteKeepAlivePingCost(d, costOf(turnID, uint64(coldPingThreshold-1), 0, 0))

	// Act.
	m.onTurnBoundary(d, false, coldPingLastTurnEnd+coldPingElapsedMs)

	// Assert: the verdict is taken synchronously on the boundary, so a decision
	// NOT to hibernate has already been taken by the time the boundary returns.
	if n := hib.writeCount(); n != 0 {
		t.Fatalf("hibernation writes = %d, want none for a ping that read its cache", n)
	}
}

// AN EXPENSIVE USER TURN IS A COST REPORT, NOT EVIDENCE THE CACHE DIED. It is
// rendered by the footer and stops nothing: the user is sitting there working,
// and a re-ingest they paid for is a fact about their prompt, not about a
// keep-alive premise.
func TestExpensiveNonPingTurnTakesNoHibernation(t *testing.T) {
	// Arrange: no ping is in flight at all.
	m, _, hib, _, _ := coldPingRig(t)
	d := controllerFor(t, m)
	m.mu.Lock()
	d.turn = turnRecord{phase: turnPhaseNamed, turnID: "req_user"}
	m.mu.Unlock()
	m.noteKeepAlivePingCost(d, costOf("req_user", uint64(coldPingThreshold*10), 0, 0))

	// Act.
	m.onTurnBoundary(d, false, coldPingLastTurnEnd+coldPingElapsedMs)

	// Assert.
	if n := hib.writeCount(); n != 0 {
		t.Fatalf("hibernation writes = %d, want none for an expensive turn nobody pinged with", n)
	}
}

// A RESULT NAMING SOME OTHER TURN CANNOT FILL THE PING'S MEASUREMENT, even
// while a ping is genuinely in flight. Attribution by elimination — "a ping was
// running, so this cost must be the ping's" — is exactly what a measurement
// that stops a session may not be built on.
func TestForeignTurnCostDoesNotFillThePingsMeasurement(t *testing.T) {
	// Arrange.
	m, _, hib, _, _ := coldPingRig(t)
	turnID := submitPingUnderTurn(t, m)
	d := controllerFor(t, m)
	m.noteKeepAlivePingCost(d, costOf(turnID+"_not_the_ping", uint64(coldPingThreshold*10), 0, 0))

	// Act.
	m.onTurnBoundary(d, false, coldPingLastTurnEnd+coldPingElapsedMs)

	// Assert.
	if n := hib.writeCount(); n != 0 {
		t.Fatalf("hibernation writes = %d, want none: the ping observed no cost of its own", n)
	}
}

// PROMPTS WAITING BEHIND THE PING MEAN THE USER IS ALREADY BACK. Their own turn
// re-warms the cache, and hibernating here would stop the shim out from under
// work the user is waiting on and then refuse the very prompts the rewind was
// on its way to deliver.
func TestColdKeepAlivePingWithPromptsWaitingTakesNoHibernation(t *testing.T) {
	// Arrange.
	m, _, hib, _, _ := coldPingRig(t)
	turnID := submitPingUnderTurn(t, m)
	d := controllerFor(t, m)
	m.mu.Lock()
	d.queue.add(&queueEntry{
		id: "q1", keepAliveHoldTurnID: turnID,
		classification: VerdictHold,
	})
	m.mu.Unlock()
	m.noteKeepAlivePingCost(d, costOf(turnID, uint64(coldPingThreshold+1), 0, 0))

	// Act.
	m.onTurnBoundary(d, false, coldPingLastTurnEnd+coldPingElapsedMs)

	// Assert.
	if n := hib.writeCount(); n != 0 {
		t.Fatalf("hibernation writes = %d, want none while the user's own prompts are waiting", n)
	}
}

// ---------------------------------------------------------------------------
// What the record carries
// ---------------------------------------------------------------------------

// THE ELAPSED IS THE ONE ACTUALLY MEASURED, taken when the ping was submitted.
// The ping's own turn end stamps the durable last-turn-end to now, so a figure
// re-derived at hibernation time would report ~0 for a session that had in fact
// been quiet for 59 minutes.
func TestColdKeepAliveHibernationCarriesTheMeasuredElapsedAndTTL(t *testing.T) {
	// Arrange.
	m, _, hib, _, _ := coldPingRig(t)
	hib.writeSeen = make(chan registry.HibernationDetail, 4)
	turnID := submitPingUnderTurn(t, m)
	d := controllerFor(t, m)
	m.noteKeepAlivePingCost(d, costOf(turnID, uint64(coldPingThreshold+1), 0, 0))
	// The ping's turn ending moves the durable instant, exactly as production's
	// TurnEndObserved does — which is what a re-derived figure would then read.
	hib.TurnEndObserved("s1", coldPingLastTurnEnd+coldPingElapsedMs)

	// Act.
	m.onTurnBoundary(d, false, coldPingLastTurnEnd+coldPingElapsedMs)

	// Assert.
	detail := awaitHibernation(t, hib)
	wantTTL := int64(coldPingCacheTTL / time.Millisecond)
	if detail.ElapsedMs != coldPingElapsedMs || detail.TTLMs != wantTTL {
		t.Fatalf("detail elapsed_ms=%d ttl_ms=%d, want the measured %d and %d",
			detail.ElapsedMs, detail.TTLMs, coldPingElapsedMs, wantTTL)
	}
}

// ---------------------------------------------------------------------------
// Ordering
// ---------------------------------------------------------------------------

// THE HIBERNATION CANNOT RACE THE PING'S OWN TEARDOWN. The measurement leaves
// with the ping's claim, in the boundary that ends its turn, so the only way to
// reach the transition is to have already retired the ping. Asserted at the
// instant the sleep is made durable rather than after the fact: an ordering
// checked afterwards is one that was allowed to be wrong in between.
func TestColdKeepAliveHibernationIsOrderedAfterThePingsTurnEnd(t *testing.T) {
	// Arrange.
	m, _, hib, windows, _ := coldPingRig(t)
	hib.writeSeen = make(chan registry.HibernationDetail, 4)
	turnID := submitPingUnderTurn(t, m)
	d := controllerFor(t, m)
	var claimHeldAtWrite, windowOpenAtWrite bool
	hib.onWrite = func() {
		_, claimHeldAtWrite = m.KeepAliveTurnID("ws")
		_, closed := windows.closed[turnID]
		windowOpenAtWrite = !closed
	}
	m.noteKeepAlivePingCost(d, costOf(turnID, uint64(coldPingThreshold+1), 0, 0))

	// Act.
	m.onTurnBoundary(d, false, coldPingLastTurnEnd+coldPingElapsedMs)
	awaitHibernation(t, hib)

	// Assert.
	if claimHeldAtWrite {
		t.Fatal("the ping still held its keep-alive claim when the sleep was recorded; the hibernation raced the turn it was decided from")
	}
	if windowOpenAtWrite {
		t.Fatal("the ping's exclusion window was still open when the sleep was recorded; the ping was not yet fully accounted for")
	}
}

// ---------------------------------------------------------------------------
// The failure path
// ---------------------------------------------------------------------------

// A HIBERNATION THAT CANNOT BE RECORDED IS SAID OUT LOUD, with everything a
// reader needs to act: the workspace, the session, the ping's turn, what it
// paid, and the threshold it crossed. There is no retry and no second route,
// because a fallback would hide a session that is provably running against a
// dead cache.
func TestColdKeepAliveHibernationFailureLogsItsFullContext(t *testing.T) {
	// Arrange.
	m, _, hib, _, capture := coldPingRig(t)
	hib.writeSeen = make(chan registry.HibernationDetail, 4)
	hib.writeErr = errors.New("state store is unavailable")
	turnID := submitPingUnderTurn(t, m)
	d := controllerFor(t, m)
	m.noteKeepAlivePingCost(d, costOf(turnID, uint64(coldPingThreshold+1), 0, 0))

	// Act.
	m.onTurnBoundary(d, false, coldPingLastTurnEnd+coldPingElapsedMs)
	awaitHibernation(t, hib)

	// Assert.
	want := []string{
		"COLD KEEP-ALIVE HIBERNATION FAILED",
		`ws="ws"`,
		"session=s1",
		"turn_id=" + turnID,
		"uncached_input_tokens=20001",
		"threshold=20000",
		"state store is unavailable",
	}
	for _, fragment := range want {
		if !capture.containsEventually(fragment) {
			t.Fatalf("the canonical failure record is missing %q", fragment)
		}
	}
}

// A FAILED DURABLE MARK MUTATES NOTHING. The record does not claim a sleep it
// could not write, so the next prompt brings the session back up rather than
// meeting a gate no record supports.
func TestColdKeepAliveHibernationFailureLeavesNoPartialState(t *testing.T) {
	// Arrange.
	m, _, hib, _, _ := coldPingRig(t)
	hib.writeSeen = make(chan registry.HibernationDetail, 4)
	hib.writeErr = errors.New("state store is unavailable")
	turnID := submitPingUnderTurn(t, m)
	d := controllerFor(t, m)
	m.noteKeepAlivePingCost(d, costOf(turnID, uint64(coldPingThreshold+1), 0, 0))

	// Act.
	m.onTurnBoundary(d, false, coldPingLastTurnEnd+coldPingElapsedMs)
	awaitHibernation(t, hib)

	// Assert.
	if detail, ok := hib.HibernationOf("s1"); ok && detail.Cause != "" {
		t.Fatalf("the record claims a sleep the registrar refused to write: %+v", detail)
	}
	if n := hib.writeCount(); n != 0 {
		t.Fatalf("recorded writes = %d, want none: the write failed", n)
	}
}

// ---------------------------------------------------------------------------
// The consumer's report
// ---------------------------------------------------------------------------

// pingResultEvent is a terminal vendor result for turnID carrying one usage
// reading.
func pingResultEvent(t *testing.T, turnID string, inputTokens, cacheCreation int64) *corev1.Event {
	t.Helper()
	msg := &datav1.ClaudeStreamMessage{Msg: &datav1.ClaudeStreamMessage_Result{
		Result: &datav1.ResultMessage{Usage: &datav1.Usage{
			InputTokens: inputTokens, CacheCreationInputTokens: cacheCreation, CacheReadInputTokens: 17202,
		}},
	}}
	vendor, err := anypb.New(msg)
	if err != nil {
		t.Fatalf("anypb.New: %v", err)
	}
	return &corev1.Event{
		SessionId: "vendor-session", ProducedAtMs: 20, RequestId: turnID,
		Payload: &corev1.Event_Vendor{Vendor: vendor},
	}
}

// THE COST IS REPORTED AGAINST THE TURN THE ACCOUNTING LEDGER ATTRIBUTED THE
// RESULT TO, with the shared uncached-input arithmetic. Reporting against a
// turn derived some other way would let the durable ledger and this report name
// different turns for one result.
func TestTerminalResultReportsItsUncachedCostAgainstTheAccountedTurn(t *testing.T) {
	// Arrange — the observed instance's own figures.
	var gotTurnID string
	var gotUncached int64
	c := newConsumer("ws", "s1", &fakePusher{}, &fakeApplier{}, nil, newFakeClearCompactStore(),
		emptyTurnAccountingStore{}, t.Logf, nil, nil, nil, nil, nil)
	c.onTurnResultCost = func(cost turnResultCost) {
		gotTurnID, gotUncached = cost.turnID, cost.expensiveInputTokens()
	}
	if err := c.Apply(&corev1.Event{
		Seq: 1, Plane: corev1.Plane_PLANE_STREAM, Class: corev1.EventClass_EVENT_CLASS_PERSISTENT,
		RequestId: "ka_1", Payload: &corev1.Event_TurnStarted{TurnStarted: &corev1.TurnStarted{TurnId: "ka_1"}},
	}); err != nil {
		t.Fatalf("Apply TurnStarted: %v", err)
	}

	// Act.
	if err := c.Consume(pingResultEvent(t, "ka_1", 2, 22862)); err != nil {
		t.Fatalf("Consume: %v", err)
	}

	// Assert.
	if gotTurnID != "ka_1" || gotUncached != 22864 {
		t.Fatalf("reported turn_id=%q uncached=%d, want %q and %d",
			gotTurnID, gotUncached, "ka_1", 22864)
	}
}

// A RESULT WITH NO USAGE REPORTS NOTHING. An absent reading is not a reading of
// zero, and a zero reported here would say the ping read its cache when nothing
// said anything at all.
func TestTerminalResultWithNoUsageReportsNoCost(t *testing.T) {
	// Arrange.
	reported := false
	c := newConsumer("ws", "s1", &fakePusher{}, &fakeApplier{}, nil, newFakeClearCompactStore(),
		emptyTurnAccountingStore{}, t.Logf, nil, nil, nil, nil, nil)
	c.onTurnResultCost = func(turnResultCost) { reported = true }
	msg := &datav1.ClaudeStreamMessage{Msg: &datav1.ClaudeStreamMessage_Result{Result: &datav1.ResultMessage{}}}
	vendor, err := anypb.New(msg)
	if err != nil {
		t.Fatalf("anypb.New: %v", err)
	}

	// Act.
	if err := c.Consume(&corev1.Event{
		SessionId: "vendor-session", ProducedAtMs: 20, RequestId: "ka_1",
		Payload: &corev1.Event_Vendor{Vendor: vendor},
	}); err != nil {
		t.Fatalf("Consume: %v", err)
	}

	// Assert.
	if reported {
		t.Fatal("a result with no usage reported a cost; an absent reading is not a reading of zero")
	}
}

// containsEventually is contains with a bounded rendezvous, yielding the
// scheduler between checks.
//
// It exists because the write seam fires when the registrar is ENTERED, which
// is one statement before the transition can have seen its error and logged it.
// The rendezvous is with that goroutine, never with the clock: the deadline is
// a test-hang backstop and is never the thing being waited on.
func (c *logCapture) containsEventually(substr string) bool {
	deadline := time.Now().Add(5 * time.Second)
	for !c.contains(substr) {
		if time.Now().After(deadline) {
			return false
		}
		runtime.Gosched()
	}
	return true
}
