package sessioncontroller

import (
	"context"
	"testing"

	corev1 "agentrepl/proto/agentshim/core/v1"

	"claude-repld/internal/errclass"
	"claude-repld/internal/shimclient"
)

// ---------------------------------------------------------------------------
// THE SECOND CHANNEL THE SAME RETIRED DEATH ARRIVES ON.
//
// THE REGRESSION: the shim persists an unexpected SDK termination as ONE
// acknowledged batch — the QueryLifecycle row and the DegradedState that
// confirms it, adjacent in the store. Every later bring-up therefore replays
// BOTH. historicalquerytermination_test.go covers the first; this file covers
// the second, which had no epoch test at all: DegradedReporter.Degraded was
// handed the payload without its envelope, so the one fact that says which
// query wrote the row never reached the classifier. A workspace whose typed
// termination was correctly withheld still died on the confirmation.
//
// The verdict is the same single classifier every other event type gets
// (turnAccountingReducer.liveEvidenceFor), asked of the envelope.
// ---------------------------------------------------------------------------

// degradationEvent is the shim's unexpected-termination confirmation at SEQ,
// written by ENVELOPEQUERY. Only the envelope classifies.
func degradationEvent(seq uint64, envelopeQuery string) *corev1.Event {
	queryID := envelopeQuery
	return &corev1.Event{
		Seq:             seq,
		QueryInstanceId: envelopeQuery,
		Payload: &corev1.Event_DegradedState{DegradedState: &corev1.DegradedState{
			Component:       shimSDKComponent,
			Reason:          "unexpected_query_termination",
			QueryInstanceId: &queryID,
		}},
	}
}

// reportDegradation feeds one DegradedState event through the reporter exactly
// as the shim client does — envelope and payload together.
func reportDegradation(t *testing.T, h *escapeHarness, ev *corev1.Event) {
	t.Helper()
	d, err := h.m.existing("ws")
	if err != nil {
		t.Fatalf("existing: %v", err)
	}
	d.consumer.Degraded("", ev, ev.GetDegradedState())
}

func TestReplayedRetiredQueryDegradationDoesNotFailTheBringUp(t *testing.T) {
	// Arrange — yesterday's query died and the store still replays the
	// confirmation half of the pair.
	h := terminationHarness(t, &fakeClient{})
	reportDegradation(t, h, degradationEvent(4142, "retired-query"))

	// Act.
	d, err := h.m.ensure(context.Background(), "ws")

	// Assert.
	if err != nil {
		t.Fatalf("ensure = %v, want the replayed retired-query degradation to stay history", err)
	}
	if d == nil {
		t.Fatal("ensure returned no session controller")
	}
}

func TestALiveQueryDegradationStillFailsTheBringUp(t *testing.T) {
	// Arrange — the SAME row, stamped by the query this bring-up created.
	// Specificity was added; live coverage was not softened.
	//
	// The client never finishes handshaking, so the gate is the ONLY way out of
	// awaitDriveable and the assertion cannot race a ready client.
	h := terminationHarness(t, blocked())
	reportDegradation(t, h, degradationEvent(4142, "live-query"))

	// Act.
	_, err := h.m.ensure(context.Background(), "ws")

	// Assert.
	if err == nil {
		t.Fatal("ensure succeeded after the LIVE query reported its SDK dead during bring-up")
	}
	if !h.log.contains("BRING-UP FAULT") {
		t.Fatalf("a live pre-readiness degradation was not reported as a bring-up fault: %v", h.log.lines)
	}
}

func TestReplayedRetiredQueryDegradationStillKeepsItsFailureCard(t *testing.T) {
	// Arrange.
	h := terminationHarness(t, &fakeClient{})

	// Act.
	reportDegradation(t, h, degradationEvent(4142, "retired-query"))

	// Assert — history keeps its account; withholding is retention, not a drop.
	if !h.hasCard(errclass.TypeUnexpectedQueryTermination) {
		t.Fatalf("replayed degradation lost its failure card: %v", h.failureCards())
	}
}

func TestReplayedRetiredQueryDegradationIsNamedInTheLog(t *testing.T) {
	// Arrange.
	h := terminationHarness(t, &fakeClient{})

	// Act.
	reportDegradation(t, h, degradationEvent(4142, "retired-query"))

	// Assert — a withheld fault that logged nothing would be a silent drop.
	if !h.log.contains("shim degradation WITHHELD from the bring-up gate") ||
		!h.log.contains("replayed_query_instance_id=retired-query") ||
		!h.log.contains(`live_query_instance_id="live-query"`) {
		t.Fatalf("the withheld replay was not named in the log: %v", h.log.lines)
	}
}

func TestReplayedRetiredQueryDegradationWithholdTakesTheInfoChannel(t *testing.T) {
	// Arrange — the durable row replays at every boot, so a warn here alarms
	// forever about a degradation that was already warned about once.
	h := terminationHarness(t, &fakeClient{})

	// Act.
	reportDegradation(t, h, degradationEvent(4142, "retired-query"))

	// Assert.
	if h.warn.contains("shim degradation WITHHELD from the bring-up gate") {
		t.Fatalf("a replayed degradation re-warned about history: %v", h.warn.lines)
	}
}

func TestReplayedRetiredQueryDegradationKeepsItsFullContextAtInfo(t *testing.T) {
	// Arrange — reclassification, not silencing: every identity field the warn
	// carried must still be on the record.
	h := terminationHarness(t, &fakeClient{})

	// Act.
	reportDegradation(t, h, degradationEvent(4142, "retired-query"))

	// Assert.
	for _, want := range []string{
		"shim degradation WITHHELD from the bring-up gate",
		"replayed_query_instance_id=retired-query",
		`live_query_instance_id="live-query"`,
		"component=claude-shim-sdk",
		"seq=4142",
		"decision=retain_history_no_bring_up_fault",
	} {
		if !h.log.contains(want) {
			t.Fatalf("the withheld degradation's record lost %q: %v", want, h.log.lines)
		}
	}
}

func TestReplayedRetiredQueryDegradationReportsItselfAsHistorical(t *testing.T) {
	// Arrange — the shim client's own relay record takes its severity from this
	// verdict and can compute nothing itself.
	h := terminationHarness(t, &fakeClient{})
	d, err := h.m.existing("ws")
	if err != nil {
		t.Fatalf("existing: %v", err)
	}
	ev := degradationEvent(4142, "retired-query")

	// Act.
	got := d.consumer.Degraded("", ev, ev.GetDegradedState())

	// Assert.
	if got != shimclient.DegradationHistorical {
		t.Fatalf("disposition = %v, want historical for a retired query's replayed row", got)
	}
}

func TestALiveQueryDegradationReportsItselfAsLive(t *testing.T) {
	// Arrange — the live arm's verdict, and therefore the relay's warn
	// severity, is untouched.
	h := terminationHarness(t, blocked())
	d, err := h.m.existing("ws")
	if err != nil {
		t.Fatalf("existing: %v", err)
	}
	ev := degradationEvent(4142, "live-query")

	// Act.
	got := d.consumer.Degraded("", ev, ev.GetDegradedState())

	// Assert.
	if got != shimclient.DegradationLive {
		t.Fatalf("disposition = %v, want live for the bound query's own row", got)
	}
}

// ---------------------------------------------------------------------------
// ONE CARD, ONE RECORD, PER REPLAYED PAIR.
//
// The pair's two halves reach two different sinks and both derive
// degradedUUID("claude-shim-sdk"), so one replayed death used to record
// "system failure … resolved=false" twice per boot. The live path has always
// collapsed that through unexpectedQueryTerminationSurfaced; the withhold path
// now does the same, keyed by the retired query so it can never swallow a live
// one.
// ---------------------------------------------------------------------------

func TestAReplayedTerminationPairRecordsOneSystemFailure(t *testing.T) {
	// Arrange — both halves of the SAME retired query's acknowledged pair, in
	// the order the store writes them.
	h := terminationHarness(t, &fakeClient{})

	// Act.
	consumeTermination(t, h, terminationEvent(4141, "retired-query"))
	reportDegradation(t, h, degradationEvent(4142, "retired-query"))

	// Assert.
	if got := h.log.count("system failure session="); got != 1 {
		t.Fatalf("system-failure records = %d, want exactly 1 for one replayed pair: %v", got, h.log.lines)
	}
}

func TestAReplayedTerminationPairPushesOneFailureCard(t *testing.T) {
	// Arrange.
	h := terminationHarness(t, &fakeClient{})

	// Act.
	consumeTermination(t, h, terminationEvent(4141, "retired-query"))
	reportDegradation(t, h, degradationEvent(4142, "retired-query"))

	// Assert.
	cards := 0
	for _, c := range h.failureCards() {
		if errclass.TypeName(c) == string(errclass.TypeUnexpectedQueryTermination) {
			cards++
		}
	}
	if cards != 1 {
		t.Fatalf("failure cards = %d, want exactly 1 for one replayed pair: %v", cards, h.failureCards())
	}
}

func TestAReplayedTerminationPairKeepsTheTypedDetail(t *testing.T) {
	// Arrange — first-wins is only correct because the store writes the RICHER
	// half first: only the QueryLifecycle row carries the typed detail, and the
	// confirming DegradedState must not clobber the card with a poorer one.
	h := terminationHarness(t, &fakeClient{})

	// Act.
	consumeTermination(t, h, terminationEvent(4141, "retired-query"))
	reportDegradation(t, h, degradationEvent(4142, "retired-query"))

	// Assert.
	cards := h.failureCards()
	if len(cards) == 0 || cards[len(cards)-1].GetKind().GetQueryTermination().GetDetail().GetQueryInstanceId() != "retired-query" {
		t.Fatalf("the replayed pair's card lost its typed query-termination detail: %v", cards)
	}
}

func TestTwoDistinctRetiredQueriesEachGetTheirOwnRecord(t *testing.T) {
	// Arrange — the latch is keyed by the retired query, not a bare bool, so a
	// second retired query's replay is its own event rather than a duplicate of
	// the first.
	h := terminationHarness(t, &fakeClient{})

	// Act.
	reportDegradation(t, h, degradationEvent(4142, "retired-query"))
	reportDegradation(t, h, degradationEvent(4143, "other-retired-query"))

	// Assert.
	if got := h.log.count("system failure session="); got != 2 {
		t.Fatalf("system-failure records = %d, want one per distinct retired query: %v", got, h.log.lines)
	}
}

func TestAReplayedPairWithNoQueryIdentityStillCards(t *testing.T) {
	// Arrange — an unkeyed latch would collapse every unidentified replay into
	// one card. Losing a card is worse than recording one twice, so an empty
	// retired-query id pushes unlatched.
	h := terminationHarness(t, &fakeClient{})
	ev := degradationEvent(4142, "retired-query")
	ev.GetDegradedState().QueryInstanceId = nil

	// Act.
	reportDegradation(t, h, ev)

	// Assert.
	if !h.hasCard(errclass.TypeUnexpectedQueryTermination) {
		t.Fatalf("an unidentified replayed degradation lost its card: %v", h.failureCards())
	}
}

func TestReplayedRetiredQueryDegradationOpensNoRuntimeFault(t *testing.T) {
	// Arrange — a fault window opened for a query that is already dead would
	// colour the workspace over a degradation that never happened to it.
	h := terminationHarness(t, &fakeClient{})

	// Act.
	reportDegradation(t, h, degradationEvent(4142, "retired-query"))

	// Assert.
	for _, edge := range h.applier.faultEdges {
		if edge.component == shimSDKComponent {
			t.Fatalf("a replayed degradation opened a runtime fault: %+v", edge)
		}
	}
}

func TestReplayedRetiredQueryDegradationDoesNotSwallowALaterLiveOne(t *testing.T) {
	// Arrange — the replay must not arm the duplicate-suppression latch, or the
	// live query's own death would be discarded as a repeat of yesterday's.
	h := terminationHarness(t, &fakeClient{})
	reportDegradation(t, h, degradationEvent(4142, "retired-query"))

	// Act.
	reportDegradation(t, h, degradationEvent(4143, "live-query"))

	// Assert.
	if h.log.contains("duplicate unexpected query termination suppressed") {
		t.Fatalf("a live SDK death was discarded as a duplicate of replayed history: %v", h.log.lines)
	}
	if !h.log.contains("BRING-UP FAULT") {
		t.Fatalf("the live SDK death never reached the bring-up gate: %v", h.log.lines)
	}
}

func TestReplayedRetiredQueryDegradationLeavesNoBringUpFailureStreak(t *testing.T) {
	// Arrange — nothing may latch: a replay that quietly counted toward the
	// give-up bound would park the workspace on the third open.
	h := terminationHarness(t, &fakeClient{})
	reportDegradation(t, h, degradationEvent(4142, "retired-query"))

	// Act.
	if _, err := h.m.ensure(context.Background(), "ws"); err != nil {
		t.Fatalf("ensure: %v", err)
	}

	// Assert.
	if got := h.m.bringUpFailuresFor("s1"); got != 0 {
		t.Fatalf("bring-up failure streak = %d, want 0 after a purely historical replay", got)
	}
}

func TestDaemonOriginatedDegradationWithoutAnEnvelopeIsLive(t *testing.T) {
	// Arrange — the daemon's own capability-channel report carries no producer
	// stamp. An unstamped degradation must fail closed as live, exactly as an
	// unstamped event does everywhere else.
	h := terminationHarness(t, &fakeClient{})
	d, err := h.m.existing("ws")
	if err != nil {
		t.Fatalf("existing: %v", err)
	}

	// Act.
	d.consumer.Degraded("", nil, &corev1.DegradedState{
		Component: "daemon-model-catalog",
		Reason:    "model catalog rejected by sink",
	})

	// Assert.
	if h.log.contains("shim degradation WITHHELD from the bring-up gate") {
		t.Fatalf("an unstamped daemon-originated degradation was waved through as history: %v", h.log.lines)
	}
}

// TestReplayedRetiredQueryDegradationWithholdsItsRecoveryToo pins the symmetry
// the withhold depends on: both edges of a retired query's fault window are
// history, so neither is applied and the pair still cancels.
func TestReplayedRetiredQueryDegradationWithholdsItsRecoveryToo(t *testing.T) {
	// Arrange.
	h := terminationHarness(t, &fakeClient{})
	recovery := degradationEvent(4143, "retired-query")
	recovery.GetDegradedState().Component = "shim-store-client"
	recovery.GetDegradedState().Reason = ""
	recovery.GetDegradedState().Recovered = true

	// Act.
	reportDegradation(t, h, recovery)

	// Assert.
	for _, edge := range h.applier.faultEdges {
		if edge.component == "shim-store-client" {
			t.Fatalf("a replayed recovery closed a fault window this process never opened: %+v", edge)
		}
	}
}
