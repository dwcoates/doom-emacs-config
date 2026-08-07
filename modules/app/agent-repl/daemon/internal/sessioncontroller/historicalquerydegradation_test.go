package sessioncontroller

import (
	"context"
	"testing"

	corev1 "agentrepl/proto/agentshim/core/v1"

	"claude-repld/internal/errclass"
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
