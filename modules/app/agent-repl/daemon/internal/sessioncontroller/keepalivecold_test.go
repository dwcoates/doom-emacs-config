package sessioncontroller

import (
	"context"
	"errors"
	"runtime"
	"testing"
	"time"

	datav1 "agentrepl/proto/agentshim/data/v1"
	frontendv1 "agentrepl/proto/frontend/v1"
	protocolv1 "agentrepl/proto/protocol/v1"

	"claude-repld/internal/keepalive"
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
	hib.TurnEndObserved("s1", coldPingLastTurnEnd, true)
	if err := m.Ensure("ws"); err != nil {
		t.Fatalf("Ensure: %v", err)
	}
	waitForWirings(applier, 1)
	m.onConnected("ws", "s1", &protocolv1.ShimHello{})
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

// coldVerdictOf reads the session's latched cold-cache verdict.
func coldVerdictOf(t *testing.T, m *Manager) *coldCacheVerdict {
	t.Helper()
	m.mu.Lock()
	defer m.mu.Unlock()
	return m.byWS["ws"].cacheProvenCold
}

// ---------------------------------------------------------------------------
// The verdict
// ---------------------------------------------------------------------------

// A PING THAT PAID FOR THE WHOLE CONVERSATION IS PROOF THE CACHE WAS GONE — AND
// PROOF OF A COST, NOT A REASON TO SLEEP. This used to hibernate the session
// outright, at roughly the ping window (about an hour) under a six-hour idle
// cutoff, which broke the contract that nothing sleeps before that cutoff.
func TestColdKeepAlivePingTakesNoHibernation(t *testing.T) {
	// Arrange.
	m, _, hib, _, _ := coldPingRig(t)
	turnID := submitPingUnderTurn(t, m)
	d := controllerFor(t, m)
	m.noteKeepAlivePingCost(d, costOf(turnID, uint64(coldPingThreshold+1), 0, 0))

	// Act.
	m.onTurnBoundary(d, false, coldPingLastTurnEnd+coldPingElapsedMs)

	// Assert.
	if n := hib.writeCount(); n != 0 {
		t.Fatalf("hibernation writes = %d, want none: a dead cache is a reason to stop spending on it, not to tear the session down", n)
	}
}

// WHAT IT DOES INSTEAD IS STOP THE SPENDING. The verdict is latched so no later
// ping pays full freight to learn the same thing.
func TestColdKeepAlivePingLatchesTheVerdict(t *testing.T) {
	// Arrange.
	m, _, _, _, _ := coldPingRig(t)
	turnID := submitPingUnderTurn(t, m)
	d := controllerFor(t, m)
	m.noteKeepAlivePingCost(d, costOf(turnID, uint64(coldPingThreshold+1), 0, 0))

	// Act.
	m.onTurnBoundary(d, false, coldPingLastTurnEnd+coldPingElapsedMs)

	// Assert.
	verdict := coldVerdictOf(t, m)
	if verdict == nil || verdict.turnID != turnID {
		t.Fatalf("cold-cache verdict = %+v, want one latched by the ping %s", verdict, turnID)
	}
}

// AND THE LATCH IS WHAT DECLINES THE NEXT PING. Without it the policy would ping
// an hour later, pay full freight again, and learn the same thing again.
func TestALatchedColdVerdictDeclinesTheNextPing(t *testing.T) {
	// Arrange.
	m, _, _, _, _ := coldPingRig(t)
	turnID := submitPingUnderTurn(t, m)
	d := controllerFor(t, m)
	m.noteKeepAlivePingCost(d, costOf(turnID, uint64(coldPingThreshold+1), 0, 0))
	m.onTurnBoundary(d, false, coldPingLastTurnEnd+coldPingElapsedMs)

	// Act.
	_, err := m.SubmitKeepAlivePing(context.Background(), "ws")

	// Assert.
	if !errors.Is(err, ErrKeepAliveNotEligible) {
		t.Fatalf("SubmitKeepAlivePing after a cold verdict = %v, want %v", err, ErrKeepAliveNotEligible)
	}
}

// THE ELIGIBILITY REFUSAL NAMES THE VERDICT, so a reader of the decline line can
// tell a proven-cold cache from a live turn or a queued prompt.
func TestTheColdVerdictDeclineNamesItself(t *testing.T) {
	// Arrange.
	m, _, _, _, _ := coldPingRig(t)
	d := controllerFor(t, m)
	m.mu.Lock()
	d.cacheProvenCold = &coldCacheVerdict{turnID: "ka_earlier"}
	m.mu.Unlock()

	// Act.
	m.mu.Lock()
	ok, why := m.keepAliveEligibleLocked(d)
	m.mu.Unlock()

	// Assert.
	if ok || why != "cache_proven_cold" {
		t.Fatalf("keepAliveEligibleLocked = (%t, %q), want (false, %q)", ok, why, "cache_proven_cold")
	}
}

// REAL WORK RETIRES THE VERDICT, because the turn it is about to run rebuilds
// the very prefix the verdict was a fact about.
func TestARealPromptRetiresTheColdVerdict(t *testing.T) {
	// Arrange.
	m, _, _, _, _ := coldPingRig(t)
	d := controllerFor(t, m)
	m.mu.Lock()
	d.cacheProvenCold = &coldCacheVerdict{turnID: "ka_earlier"}
	m.mu.Unlock()

	// Act.
	if err := m.SubmitPrompt(context.Background(), "ws", "req_user", "hello", "", protocolv1.PromptOrigin_PROMPT_ORIGIN_USER_SENT); err != nil {
		t.Fatalf("SubmitPrompt: %v", err)
	}

	// Assert.
	if verdict := coldVerdictOf(t, m); verdict != nil {
		t.Fatalf("cold-cache verdict = %+v, want it retired by a real prompt", verdict)
	}
}

// A PING THAT CAME BACK CHEAP PROVES THE OPPOSITE, and the opposite of a cold
// ping is the feature working. Nothing is latched.
func TestWarmKeepAlivePingLatchesNoVerdict(t *testing.T) {
	// Arrange.
	m, _, _, _, _ := coldPingRig(t)
	turnID := submitPingUnderTurn(t, m)
	d := controllerFor(t, m)
	m.noteKeepAlivePingCost(d, costOf(turnID, uint64(coldPingThreshold-1), 0, 0))

	// Act.
	m.onTurnBoundary(d, false, coldPingLastTurnEnd+coldPingElapsedMs)

	// Assert.
	if verdict := coldVerdictOf(t, m); verdict != nil {
		t.Fatalf("cold-cache verdict = %+v, want none for a ping that read its cache", verdict)
	}
}

// AN EXPENSIVE USER TURN IS A COST REPORT, NOT EVIDENCE THE CACHE DIED. It is
// rendered by the footer and stops nothing: the user is sitting there working,
// and a re-ingest they paid for is a fact about their prompt, not about a
// keep-alive premise.
func TestExpensiveNonPingTurnLatchesNoVerdict(t *testing.T) {
	// Arrange: no ping is in flight at all.
	m, _, _, _, _ := coldPingRig(t)
	d := controllerFor(t, m)
	m.mu.Lock()
	d.turn = turnRecord{phase: turnPhaseNamed, turnID: "req_user"}
	m.mu.Unlock()
	m.noteKeepAlivePingCost(d, costOf("req_user", uint64(coldPingThreshold*10), 0, 0))

	// Act.
	m.onTurnBoundary(d, false, coldPingLastTurnEnd+coldPingElapsedMs)

	// Assert.
	if verdict := coldVerdictOf(t, m); verdict != nil {
		t.Fatalf("cold-cache verdict = %+v, want none for an expensive turn nobody pinged with", verdict)
	}
}

// A RESULT NAMING SOME OTHER TURN CANNOT FILL THE PING'S MEASUREMENT, even
// while a ping is genuinely in flight. Attribution by elimination — "a ping was
// running, so this cost must be the ping's" — is exactly what a measurement
// that changes policy may not be built on.
func TestForeignTurnCostDoesNotFillThePingsMeasurement(t *testing.T) {
	// Arrange.
	m, _, _, _, _ := coldPingRig(t)
	turnID := submitPingUnderTurn(t, m)
	d := controllerFor(t, m)
	m.noteKeepAlivePingCost(d, costOf(turnID+"_not_the_ping", uint64(coldPingThreshold*10), 0, 0))

	// Act.
	m.onTurnBoundary(d, false, coldPingLastTurnEnd+coldPingElapsedMs)

	// Assert.
	if verdict := coldVerdictOf(t, m); verdict != nil {
		t.Fatalf("cold-cache verdict = %+v, want none: the ping observed no cost of its own", verdict)
	}
}

// PROMPTS WAITING BEHIND THE PING MEAN THE USER IS ALREADY BACK. Their own turn
// re-warms the cache, so there is nothing to stop spending on.
func TestColdKeepAlivePingWithPromptsWaitingLatchesNoVerdict(t *testing.T) {
	// Arrange.
	m, _, _, _, _ := coldPingRig(t)
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
	if verdict := coldVerdictOf(t, m); verdict != nil {
		t.Fatalf("cold-cache verdict = %+v, want none while the user's own prompts are waiting", verdict)
	}
}

// ---------------------------------------------------------------------------
// What the verdict carries
// ---------------------------------------------------------------------------

// THE ELAPSED IS THE ONE ACTUALLY MEASURED, taken when the ping was submitted.
// The ping's own turn end stamps the durable last-turn-end to now, so a figure
// re-derived at verdict time would report ~0 for a session that had in fact
// been quiet for 59 minutes.
func TestColdKeepAliveVerdictCarriesTheMeasuredElapsedAndTTL(t *testing.T) {
	// Arrange.
	m, _, hib, _, _ := coldPingRig(t)
	turnID := submitPingUnderTurn(t, m)
	d := controllerFor(t, m)
	m.noteKeepAlivePingCost(d, costOf(turnID, uint64(coldPingThreshold+1), 0, 0))
	// The ping's turn ending moves the CACHE clock, exactly as production's
	// TurnEndObserved does — which is what a re-derived figure would then read.
	// It moves no engagement clock, because a ping is not somebody using the
	// workspace.
	hib.TurnEndObserved("s1", coldPingLastTurnEnd+coldPingElapsedMs, false)

	// Act.
	m.onTurnBoundary(d, false, coldPingLastTurnEnd+coldPingElapsedMs)

	// Assert.
	verdict := coldVerdictOf(t, m)
	wantTTL := int64(coldPingCacheTTL / time.Millisecond)
	if verdict == nil || verdict.elapsedMs != coldPingElapsedMs || verdict.ttlMs != wantTTL {
		t.Fatalf("verdict = %+v, want the measured elapsed %d and ttl %d", verdict, coldPingElapsedMs, wantTTL)
	}
}

// ---------------------------------------------------------------------------
// Ordering
// ---------------------------------------------------------------------------

// THE VERDICT CANNOT RACE THE PING'S OWN TEARDOWN. The measurement leaves with
// the ping's claim, in the boundary that ends its turn, so a latched verdict
// implies a retired ping.
func TestColdKeepAliveVerdictIsTakenAfterThePingsTurnEnd(t *testing.T) {
	// Arrange.
	m, _, _, windows, _ := coldPingRig(t)
	turnID := submitPingUnderTurn(t, m)
	d := controllerFor(t, m)
	m.noteKeepAlivePingCost(d, costOf(turnID, uint64(coldPingThreshold+1), 0, 0))

	// Act.
	m.onTurnBoundary(d, false, coldPingLastTurnEnd+coldPingElapsedMs)

	// Assert.
	if coldVerdictOf(t, m) == nil {
		t.Fatal("no verdict was latched; the rest of this assertion would be vacuous")
	}
	if _, held := m.KeepAliveTurnID("ws"); held {
		t.Fatal("the ping still held its keep-alive claim after the verdict; the verdict raced the turn it was decided from")
	}
	if _, closed := windows.closed[turnID]; !closed {
		t.Fatal("the ping's exclusion window was still open after the verdict; the ping was not yet fully accounted for")
	}
}

// ---------------------------------------------------------------------------
// The consumer's report
// ---------------------------------------------------------------------------

// pingResultEvent is a terminal vendor result for turnID carrying one usage
// reading.
func pingResultEvent(t *testing.T, turnID string, inputTokens, cacheCreation int64) *protocolv1.Event {
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
	return &protocolv1.Event{
		SessionId: "vendor-session", ProducedAtMs: 20, RequestId: turnID,
		Payload: &protocolv1.Event_Vendor{Vendor: vendor},
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
	if err := c.Apply(&protocolv1.Event{
		Seq: 1, Plane: protocolv1.Plane_PLANE_STREAM, Class: protocolv1.EventClass_EVENT_CLASS_PERSISTENT,
		RequestId: "ka_1", Payload: &protocolv1.Event_TurnStarted{TurnStarted: &protocolv1.TurnStarted{TurnId: "ka_1"}},
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
	if err := c.Consume(&protocolv1.Event{
		SessionId: "vendor-session", ProducedAtMs: 20, RequestId: "ka_1",
		Payload: &protocolv1.Event_Vendor{Vendor: vendor},
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
