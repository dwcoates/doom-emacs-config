package sessioncontroller

import (
	"strings"
	"testing"

	corev1 "agentrepl/proto/agentshim/core/v1"
)

// boundReducer is a reducer bound to a live query created at seq 100, which is
// the shape a resuming daemon has while it replays rows beneath that boundary.
func boundReducer(t *testing.T) *turnAccountingReducer {
	t.Helper()
	r := newTurnAccountingReducer()
	if err := r.bindHandshakeIdentity(&corev1.ShimHello{
		QueryInstanceId: "live-query",
		QueryCreatedSeq: 100,
		VendorSessionId: "vendor",
	}); err != nil {
		t.Fatalf("bind handshake: %v", err)
	}
	return r
}

// observationEvent is boundary evidence at SEQ stamped with QUERY.
func observationEvent(seq uint64, query, turnID string) *corev1.Event {
	return &corev1.Event{
		Seq: seq,
		Payload: &corev1.Event_AccountUsageObservation{
			AccountUsageObservation: &corev1.AccountUsageObservation{
				QueryInstanceId: query,
				TurnId:          turnID,
				Boundary:        &corev1.AccountUsageObservation_TurnStart{TurnStart: &corev1.TurnStartUsageBoundary{}},
			},
		},
	}
}

// THE REGRESSION: a retired query's observation, replayed beneath the live
// query's creation boundary, was judged against the live id and rejected. The
// rejection is terminal, so one such row made its workspace permanently
// unopenable — the stored id is frozen while the live id is minted fresh on
// every bring-up, so the two can never agree.
func TestRetiredQueryObservationIsNotRejected(t *testing.T) {
	// Arrange
	r := boundReducer(t)

	// Act — seq 84 is beneath the live query's creation at 100.
	err := r.observe(observationEvent(84, "retired-query", "turn-1"), "daemon-session")

	// Assert
	if err != nil {
		t.Fatalf("replayed retired-query observation = %v, want it retained as history", err)
	}
}

// It is retained as HISTORY, not admitted as live evidence — the same terms the
// lifecycle rows around it already get.
func TestRetiredQueryObservationIsNotAdmittedAsEvidence(t *testing.T) {
	// Arrange
	r := boundReducer(t)
	r.turns["turn-1"] = &accountingTurn{}

	// Act
	if err := r.observe(observationEvent(84, "retired-query", "turn-1"), "daemon-session"); err != nil {
		t.Fatalf("observe: %v", err)
	}

	// Assert
	if r.turns["turn-1"].startUsage != nil {
		t.Fatal("retired-query evidence was admitted as the live turn's start boundary")
	}
}

// THE CHECK STILL EARNS ITS PLACE. An observation from a foreign query AT OR
// BEYOND the live creation boundary is a live protocol contradiction — a
// retired shim injecting evidence for the running turn — and stays a hard
// failure.
func TestForeignObservationAtTheLiveBoundaryStillFails(t *testing.T) {
	// Arrange
	r := boundReducer(t)

	// Act — seq 100 is the live query's own creation boundary.
	err := r.observe(observationEvent(100, "retired-query", "turn-1"), "daemon-session")

	// Assert
	if err == nil || !strings.Contains(err.Error(), "does not match authoritative query_instance_id") {
		t.Fatalf("observation at the live boundary = %v, want the evidence-contract rejection", err)
	}
}

// The live query's own observation is unaffected beneath the boundary too: the
// exemption keys on a DIFFERENT query id, not on sequence alone.
func TestLiveQueryObservationBeneathTheBoundaryIsStillValidated(t *testing.T) {
	// Arrange
	r := boundReducer(t)

	// Act — the live query's id, so not historical; its turn is unknown.
	err := r.observe(observationEvent(84, "live-query", "unknown-turn"), "daemon-session")

	// Assert — it reached the turn lookup rather than being waved through.
	if err == nil || !strings.Contains(err.Error(), "unknown-turn") {
		t.Fatalf("live-query observation = %v, want it validated and its turn resolved", err)
	}
}

// With no creation boundary announced, nothing is admitted as historical: a
// zero boundary means the stream does not carry this query's creation record,
// so the reducer cannot prove any row predates it.
func TestNoCreationBoundaryAdmitsNoHistory(t *testing.T) {
	// Arrange
	r := newTurnAccountingReducer()
	if err := r.bindHandshakeIdentity(&corev1.ShimHello{
		QueryInstanceId: "live-query",
		VendorSessionId: "vendor",
	}); err != nil {
		t.Fatalf("bind handshake: %v", err)
	}

	// Act
	err := r.observe(observationEvent(84, "retired-query", "turn-1"), "daemon-session")

	// Assert
	if err == nil || !strings.Contains(err.Error(), "does not match authoritative query_instance_id") {
		t.Fatalf("observation with no creation boundary = %v, want the rejection", err)
	}
}
