package sessioncontroller

import (
	"context"
	"testing"

	corev1 "agentrepl/proto/agentshim/core/v1"

	"claude-repld/internal/errclass"
)

// ---------------------------------------------------------------------------
// A RETIRED QUERY'S DEATH IS HISTORY, NOT THIS BRING-UP'S FAILURE.
//
// THE REGRESSION: the shim persists an SDK termination as a durable
// QueryTerminated/DegradedState pair, so the store replays it at every later
// bring-up. The bring-up gate took that replay at face value and killed each
// fresh attempt on a fault that predated it — before the handshake could land.
// The give-up park then expired, retried, and died on the same seq. Nothing
// could clear it, because the evidence was durable: the workspace was bricked.
//
// The verdict is the one every other event type already gets from the single
// classifier (turnAccountingReducer.liveEvidenceFor): the envelope's query
// stamp against the query the handshake bound. See historicalobservation_test.go
// for the observation and lifecycle-rebind members of the same family.
// ---------------------------------------------------------------------------

// terminationHarness is a manager whose "ws" bring-up will wire, holding a
// controller whose accounting is bound to LIVE-QUERY — the shape a resuming
// daemon has while the store replays a retired query's rows at it.
func terminationHarness(t *testing.T, clients ...*fakeClient) *escapeHarness {
	t.Helper()
	h := newEscapeHarness(t, clients...)
	h.spawner.resume["s1"] = "vendor-1"
	if _, err := h.m.bringUp("ws"); err != nil {
		t.Fatalf("bringUp: %v", err)
	}
	d, err := h.m.existing("ws")
	if err != nil {
		t.Fatalf("existing: %v", err)
	}
	if err := d.consumer.accounting.bindHandshakeIdentity(&corev1.ShimHello{
		QueryInstanceId: "live-query",
		QueryCreatedSeq: 1,
		VendorSessionId: "vendor-1",
	}); err != nil {
		t.Fatalf("bind handshake: %v", err)
	}
	return h
}

// terminationEvent is an SDK-iterator death at SEQ written by ENVELOPEQUERY.
// Only the envelope classifies: it says which query WROTE the row.
func terminationEvent(seq uint64, envelopeQuery string) *corev1.Event {
	return &corev1.Event{
		Seq:             seq,
		QueryInstanceId: envelopeQuery,
		Payload: &corev1.Event_QueryLifecycle{QueryLifecycle: &corev1.QueryLifecycle{
			QueryInstanceId: envelopeQuery,
			ObservedAtMs:    4242,
			Event: &corev1.QueryLifecycle_Terminated{Terminated: &corev1.QueryTerminated{
				VendorIdentity: &corev1.QueryTerminated_VendorSessionId{VendorSessionId: "vendor-1"},
				Reason: &corev1.QueryTerminated_IteratorFailure{IteratorFailure: &corev1.QueryIteratorFailure{
					Cause: `SDK emitted TaskEnded for unknown task "a4699ecc217adfa70"`,
				}},
			}},
		}},
	}
}

func consumeTermination(t *testing.T, h *escapeHarness, ev *corev1.Event) {
	t.Helper()
	d, err := h.m.existing("ws")
	if err != nil {
		t.Fatalf("existing: %v", err)
	}
	if err := d.consumer.Consume(ev); err != nil {
		t.Fatalf("consume termination: %v", err)
	}
}

func TestReplayedRetiredQueryTerminationDoesNotFailTheBringUp(t *testing.T) {
	// Arrange — yesterday's query died and the store still holds the proof.
	h := terminationHarness(t, &fakeClient{})
	consumeTermination(t, h, terminationEvent(4141, "retired-query"))

	// Act.
	d, err := h.m.ensure(context.Background(), "ws")

	// Assert.
	if err != nil {
		t.Fatalf("ensure = %v, want the replayed retired-query death to stay history", err)
	}
	if d == nil {
		t.Fatal("ensure returned no session controller")
	}
}

func TestALiveQueryTerminationStillFailsTheBringUp(t *testing.T) {
	// Arrange — the SAME row, stamped by the query this bring-up actually
	// created. Specificity was added; nothing was softened.
	//
	// The client never finishes handshaking, so the gate is the ONLY way out of
	// awaitDriveable and the assertion cannot race a ready client.
	h := terminationHarness(t, blocked())
	consumeTermination(t, h, terminationEvent(4141, "live-query"))

	// Act.
	_, err := h.m.ensure(context.Background(), "ws")

	// Assert.
	if err == nil {
		t.Fatal("ensure succeeded after the LIVE query's SDK stream died during bring-up")
	}
	if !h.log.contains("BRING-UP FAULT") {
		t.Fatalf("a live pre-readiness SDK death was not reported as a bring-up fault: %v", h.log.lines)
	}
}

func TestReplayedRetiredQueryTerminationStillKeepsItsFailureCard(t *testing.T) {
	// Arrange.
	h := terminationHarness(t, &fakeClient{})

	// Act.
	consumeTermination(t, h, terminationEvent(4141, "retired-query"))

	// Assert — history keeps its account; the user can still read what killed
	// the previous query.
	if !h.hasCard(errclass.TypeUnexpectedQueryTermination) {
		t.Fatalf("replayed termination lost its failure card: %v", h.failureCards())
	}
}

func TestReplayedRetiredQueryTerminationIsNamedInTheLog(t *testing.T) {
	// Arrange.
	h := terminationHarness(t, &fakeClient{})

	// Act.
	consumeTermination(t, h, terminationEvent(4141, "retired-query"))

	// Assert — a withheld fault that logged nothing would be a silent drop.
	if !h.log.contains("typed query termination WITHHELD from the bring-up gate") ||
		!h.log.contains(`replayed_query_instance_id=retired-query`) ||
		!h.log.contains(`live_query_instance_id="live-query"`) {
		t.Fatalf("the withheld replay was not named in the log: %v", h.log.lines)
	}
}

func TestReplayedRetiredQueryTerminationDoesNotSwallowALaterLiveOne(t *testing.T) {
	// Arrange — the replay must not arm the duplicate-suppression latch, or the
	// live query's own death would be discarded as a repeat of yesterday's.
	h := terminationHarness(t, &fakeClient{})
	consumeTermination(t, h, terminationEvent(4141, "retired-query"))
	d, err := h.m.existing("ws")
	if err != nil {
		t.Fatalf("existing: %v", err)
	}

	// Act.
	queryID := "live-query"
	d.consumer.Degraded("", nil, &corev1.DegradedState{
		Component:       shimSDKComponent,
		Reason:          "unexpected_query_termination",
		QueryInstanceId: &queryID,
	})

	// Assert.
	if h.log.contains("duplicate unexpected query termination suppressed") {
		t.Fatalf("a live SDK death was discarded as a duplicate of replayed history: %v", h.log.lines)
	}
	if !h.log.contains("BRING-UP FAULT") {
		t.Fatalf("the live SDK death never reached the bring-up gate: %v", h.log.lines)
	}
}

func TestReplayedRetiredQueryTerminationLeavesNoBringUpFailureStreak(t *testing.T) {
	// Arrange — nothing may latch: a replay that quietly counted toward the
	// give-up bound would park the workspace on the third open.
	h := terminationHarness(t, &fakeClient{})
	consumeTermination(t, h, terminationEvent(4141, "retired-query"))

	// Act.
	if _, err := h.m.ensure(context.Background(), "ws"); err != nil {
		t.Fatalf("ensure: %v", err)
	}

	// Assert.
	if got := h.m.bringUpFailuresFor("s1"); got != 0 {
		t.Fatalf("bring-up failure streak = %d, want 0 after a purely historical replay", got)
	}
}
