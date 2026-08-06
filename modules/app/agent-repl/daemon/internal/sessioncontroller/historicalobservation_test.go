package sessioncontroller

import (
	"strings"
	"testing"

	corev1 "agentrepl/proto/agentshim/core/v1"
)

// boundReducer is a reducer bound to the live query, which is the shape a
// resuming daemon has while the store replays rows at it.
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

// observationEvent is boundary evidence at SEQ produced by ENVELOPEQUERY and
// describing PAYLOADQUERY.
//
// The two ids are supplied separately on purpose. The envelope says which query
// WROTE the row; the payload says which query the observation is ABOUT. Only
// the envelope classifies.
func observationEvent(seq uint64, envelopeQuery, payloadQuery, turnID string) *corev1.Event {
	return &corev1.Event{
		Seq:             seq,
		QueryInstanceId: envelopeQuery,
		Payload: &corev1.Event_AccountUsageObservation{
			AccountUsageObservation: &corev1.AccountUsageObservation{
				QueryInstanceId: payloadQuery,
				TurnId:          turnID,
				Boundary:        &corev1.AccountUsageObservation_TurnStart{TurnStart: &corev1.TurnStartUsageBoundary{}},
			},
		},
	}
}

// lifecycleEvent is a QueryCreated row at SEQ produced by ENVELOPEQUERY and
// describing PAYLOADQUERY.
func lifecycleEvent(seq uint64, envelopeQuery, payloadQuery string) *corev1.Event {
	return &corev1.Event{
		Seq:             seq,
		QueryInstanceId: envelopeQuery,
		Payload: &corev1.Event_QueryLifecycle{QueryLifecycle: &corev1.QueryLifecycle{
			QueryInstanceId: payloadQuery,
			Event:           &corev1.QueryLifecycle_Created{Created: &corev1.QueryCreated{}},
		}},
	}
}

// --- (b) a different query is history ----------------------------------------

// THE REGRESSION: a retired query's observation, replayed at a daemon that had
// bound a new query, was judged against the live id and rejected. The rejection
// is terminal, so one such row made its workspace permanently unopenable — the
// stored id is frozen while the live id is minted fresh on every bring-up, so
// the two can never agree.
func TestRetiredQueryObservationIsNotRejected(t *testing.T) {
	// Arrange
	r := boundReducer(t)

	// Act
	err := r.observe(observationEvent(84, "retired-query", "retired-query", "turn-1"), "daemon-session")

	// Assert
	if err != nil {
		t.Fatalf("replayed retired-query observation = %v, want it retained as history", err)
	}
}

// It is retained as HISTORY and mutates NO live state — the same terms the
// lifecycle rows around it already get.
func TestRetiredQueryObservationIsNotAdmittedAsEvidence(t *testing.T) {
	// Arrange
	r := boundReducer(t)
	r.turns["turn-1"] = &accountingTurn{}

	// Act
	if err := r.observe(observationEvent(84, "retired-query", "retired-query", "turn-1"), "daemon-session"); err != nil {
		t.Fatalf("observe: %v", err)
	}

	// Assert
	if r.turns["turn-1"].startUsage != nil {
		t.Fatal("retired-query evidence was admitted as the live turn's start boundary")
	}
}

// A retired query's LIFECYCLE row is history on the same single comparison, and
// must not rebind the reducer's authoritative query.
func TestRetiredQueryLifecycleDoesNotRebindTheLiveQuery(t *testing.T) {
	// Arrange
	r := boundReducer(t)

	// Act
	if err := r.observe(lifecycleEvent(84, "retired-query", "retired-query"), "daemon-session"); err != nil {
		t.Fatalf("replayed retired-query lifecycle = %v, want it retained as history", err)
	}

	// Assert
	if r.queryID != "live-query" {
		t.Fatalf("bound query = %q, want the live query untouched by a historical row", r.queryID)
	}
}

// --- (a) the live query still gets the full, fatal check ----------------------

// THE CHECK STILL EARNS ITS PLACE. A row PRODUCED BY the live query that
// contradicts the live query in its payload is a genuine protocol
// contradiction — a retired shim injecting evidence for the running turn — and
// stays a hard failure.
func TestLiveQueryObservationWithAForeignPayloadStillFails(t *testing.T) {
	// Arrange
	r := boundReducer(t)

	// Act — the ENVELOPE names the live query, so the check applies in full.
	err := r.observe(observationEvent(100, "live-query", "retired-query", "turn-1"), "daemon-session")

	// Assert
	if err == nil || !strings.Contains(err.Error(), "does not match authoritative query_instance_id") {
		t.Fatalf("live-produced observation = %v, want the evidence-contract rejection", err)
	}
}

// A live row naming an unknown turn is still validated all the way to the turn
// lookup rather than waved through.
func TestLiveQueryObservationIsStillValidated(t *testing.T) {
	// Arrange
	r := boundReducer(t)

	// Act
	err := r.observe(observationEvent(84, "live-query", "live-query", "unknown-turn"), "daemon-session")

	// Assert
	if err == nil || !strings.Contains(err.Error(), "unknown-turn") {
		t.Fatalf("live-query observation = %v, want it validated and its turn resolved", err)
	}
}

// A live LIFECYCLE row whose payload contradicts the bound query is fatal.
func TestLiveLifecycleWithAForeignPayloadStillFails(t *testing.T) {
	// Arrange
	r := boundReducer(t)

	// Act
	err := r.observe(lifecycleEvent(100, "live-query", "some-other-query"), "daemon-session")

	// Assert
	if err == nil || !strings.Contains(err.Error(), "does not match bound query_instance_id") {
		t.Fatalf("live-produced lifecycle = %v, want the bound-identity rejection", err)
	}
}

// --- (c) an empty stamp behaves exactly as a live one -------------------------

// FAIL CLOSED. A producer that predates query_instance_id stamps nothing, and
// such a row must keep precisely the behavior it had before the field existed:
// judged against the live query and fatal on a contradiction.
func TestUnstampedObservationIsTreatedAsLive(t *testing.T) {
	// Arrange
	r := boundReducer(t)

	// Act — empty envelope stamp, contradictory payload.
	err := r.observe(observationEvent(84, "", "retired-query", "turn-1"), "daemon-session")

	// Assert
	if err == nil || !strings.Contains(err.Error(), "does not match authoritative query_instance_id") {
		t.Fatalf("unstamped observation = %v, want it judged exactly as a live row is", err)
	}
}

// The same, one comparison down: an unstamped row is never classified historical.
func TestUnstampedRowIsNeverHistorical(t *testing.T) {
	// Arrange
	r := boundReducer(t)

	// Act
	_, historical := r.liveEvidenceFor(observationEvent(84, "", "retired-query", "turn-1"))

	// Assert
	if historical {
		t.Fatal("an unstamped row was classified historical; empty must fail closed")
	}
}

// --- (d) THE STARTUP CASE -----------------------------------------------------

// A session's own first QueryCreated is written to the store BEFORE its
// subscription exists — the subscription needs a from_seq only the daemon
// supplies later in the handshake — so the store serves that row back during
// catch-up on every replay.
//
// It describes the query running RIGHT NOW. Classifying by delivery calls it
// replayed and needs a second fact to recover the truth; classifying by the
// envelope gets it right with ONE comparison, which is what this asserts.
func TestOwnStartupQueryCreatedIsNotHistory(t *testing.T) {
	// Arrange — a reducer bound to the live query, as after the handshake.
	r := boundReducer(t)

	// Act — the session's own QueryCreated, at a seq beneath everything the
	// live connection has seen, arriving during catch-up.
	_, historical := r.liveEvidenceFor(lifecycleEvent(1, "live-query", "live-query"))

	// Assert — one comparison, no companion condition.
	if historical {
		t.Fatal("the session's own startup QueryCreated was classified as history")
	}
}

// And it is admitted, not merely classified: the reducer accepts it and keeps
// its binding.
func TestOwnStartupQueryCreatedIsAdmitted(t *testing.T) {
	// Arrange
	r := boundReducer(t)

	// Act
	err := r.observe(lifecycleEvent(1, "live-query", "live-query"), "daemon-session")

	// Assert
	if err != nil {
		t.Fatalf("own startup QueryCreated = %v, want it admitted as live", err)
	}
	if r.queryID != "live-query" {
		t.Fatalf("bound query = %q, want it unchanged", r.queryID)
	}
}

// --- one classifier, every event type ----------------------------------------

// THE A4 PROPERTY: the verdict depends on the producing query, never on which
// event type happens to be asking. Two event types with the same envelope stamp
// must be classified identically — the divergence that used to exist between
// them is what bricked a workspace.
func TestTheClassificationDoesNotDependOnTheEventType(t *testing.T) {
	// Arrange — one retired-query row, two shapes of it.
	r := boundReducer(t)

	// Act.
	_, lifecycleHistorical := r.liveEvidenceFor(lifecycleEvent(84, "retired-query", "retired-query"))
	_, observationHistorical := r.liveEvidenceFor(observationEvent(84, "retired-query", "retired-query", "turn-1"))

	// Assert.
	if lifecycleHistorical != observationHistorical {
		t.Fatalf("lifecycle historical=%v but observation historical=%v for the same producing query — this divergence is the defect",
			lifecycleHistorical, observationHistorical)
	}
	if !lifecycleHistorical {
		t.Fatal("a retired-query row was not classified historical")
	}
}

// THE ENVELOPE IS THE SOURCE, NOT THE PAYLOAD. A row the LIVE query produced
// while describing a retired one is live evidence; digging the id out of the
// payload is exactly the substitution this field exists to remove.
func TestClassificationReadsTheEnvelopeNotThePayload(t *testing.T) {
	// Arrange
	r := boundReducer(t)

	// Act
	_, historical := r.liveEvidenceFor(observationEvent(84, "live-query", "retired-query", "turn-1"))

	// Assert
	if historical {
		t.Fatal("a row PRODUCED BY the live query was classified historical from its payload")
	}
}

// Historical rows yield NO proof, which is what stops them reaching a
// live-identity comparison at all.
func TestHistoricalRowsCarryNoLiveEvidence(t *testing.T) {
	// Arrange.
	r := boundReducer(t)

	// Act.
	live, historical := r.liveEvidenceFor(observationEvent(84, "retired-query", "retired-query", "turn-1"))

	// Assert.
	if !historical || live.queryID != "" {
		t.Fatalf("historical=%v live=%+v, want historical with empty proof", historical, live)
	}
}

// The zero value fails CLOSED: a hand-built proof matches nothing and is
// rejected for lacking an authoritative id, rather than passing validation.
func TestZeroLiveEvidenceFailsClosed(t *testing.T) {
	// Arrange / Act.
	err := validateAccountUsageObservation(liveEvidence{}, "t", &corev1.AccountUsageObservation{
		QueryInstanceId: "anything", TurnId: "t",
	})

	// Assert.
	if err == nil || !strings.Contains(err.Error(), "authoritative query_instance_id is required") {
		t.Fatalf("zero-value evidence = %v, want the authoritative-id refusal", err)
	}
}

// A live row still yields proof carrying the live id, so genuine contradictions
// are still caught.
func TestLiveRowsCarryTheLiveQueryId(t *testing.T) {
	// Arrange.
	r := boundReducer(t)

	// Act.
	live, historical := r.liveEvidenceFor(observationEvent(100, "live-query", "retired-query", "turn-1"))

	// Assert.
	if historical || live.queryID != "live-query" {
		t.Fatalf("historical=%v live=%+v, want live proof carrying the live query id", historical, live)
	}
}

// An UNBOUND reducer has nothing to compare against, so it admits no history.
func TestUnboundReducerAdmitsNoHistory(t *testing.T) {
	// Arrange
	r := newTurnAccountingReducer()

	// Act
	_, historical := r.liveEvidenceFor(observationEvent(84, "retired-query", "retired-query", "turn-1"))

	// Assert
	if historical {
		t.Fatal("an unbound reducer classified a row historical with nothing to compare against")
	}
}
