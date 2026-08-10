package sessioncontroller

import (
	"errors"
	"testing"

	corev1 "agentrepl/proto/agentshim/core/v1"
)

func queryCreatedEvent(queryID, requested, envelopeSessionID string, seq uint64) *corev1.Event {
	created := &corev1.QueryCreated{Invocation: &corev1.QueryCreated_Fresh{Fresh: &corev1.FreshQuery{}}}
	if requested != "" {
		created.Invocation = &corev1.QueryCreated_Resumed{Resumed: &corev1.ResumedQuery{RequestedVendorSessionId: requested}}
	}
	return &corev1.Event{
		SessionId: envelopeSessionID,
		Seq:       seq,
		Payload: &corev1.Event_QueryLifecycle{QueryLifecycle: &corev1.QueryLifecycle{
			QueryInstanceId: queryID,
			Event:           &corev1.QueryLifecycle_Created{Created: created},
		}},
	}
}

func queryRuntimeEvent(queryID, observed, envelopeSessionID string, seq uint64) *corev1.Event {
	return &corev1.Event{
		SessionId: envelopeSessionID,
		Seq:       seq,
		Payload: &corev1.Event_QueryLifecycle{QueryLifecycle: &corev1.QueryLifecycle{
			QueryInstanceId: queryID,
			Event: &corev1.QueryLifecycle_RuntimeObserved{RuntimeObserved: &corev1.QueryRuntimeObserved{
				Identity: &corev1.QueryRuntimeIdentity{VendorSessionId: observed},
			}},
		}},
	}
}

func TestResumeIdentityTrackerKeepsResumedIdentityMismatchFatalOnReplay(t *testing.T) {
	tracker := newResumeIdentityTracker()
	if mismatch, _, err := tracker.observe(queryCreatedEvent("query", "requested", "requested", 1)); err != nil || mismatch != nil {
		t.Fatalf("created = (%+v, %v)", mismatch, err)
	}
	mismatch, _, err := tracker.observe(queryRuntimeEvent("query", "replacement", "requested", 2))
	if err != nil || mismatch == nil || mismatch.requestedVendorSessionID != "requested" || mismatch.observedVendorSessionID != "replacement" {
		t.Fatalf("runtime = (%+v, %v)", mismatch, err)
	}
	if repeated, _, err := tracker.observe(queryRuntimeEvent("query", "replacement", "requested", 2)); err != nil || repeated == nil {
		t.Fatalf("repeated runtime = (%+v, %v), want the mismatch to remain fatal", repeated, err)
	}
}

func TestResumeIdentityTrackerAcceptsMatchingResumeAndFreshIdentity(t *testing.T) {
	tests := []struct {
		name      string
		requested string
		observed  string
	}{
		{name: "matching resume", requested: "vendor", observed: "vendor"},
		{name: "fresh query", observed: "vendor"},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			tracker := newResumeIdentityTracker()
			if mismatch, _, err := tracker.observe(queryCreatedEvent("query", tc.requested, tc.requested, 1)); err != nil || mismatch != nil {
				t.Fatalf("created = (%+v, %v)", mismatch, err)
			}
			if mismatch, _, err := tracker.observe(queryRuntimeEvent("query", tc.observed, tc.observed, 2)); err != nil || mismatch != nil {
				t.Fatalf("runtime = (%+v, %v)", mismatch, err)
			}
		})
	}
}

func TestConsumerTerminatesOnTypedResumeIdentityMismatchBeforeMutation(t *testing.T) {
	push := &fakePusher{}
	c := newConsumer("ws", "agent-session", push, &fakeApplier{}, nil,
		newFakeClearCompactStore(), emptyTurnAccountingStore{}, t.Logf,
		nil, nil, nil, nil, nil)
	var observed []string
	c.onVendorSessionID = func(id string) { observed = append(observed, id) }

	if err := c.Consume(queryCreatedEvent("query", "requested", "requested", 1)); err != nil {
		t.Fatal(err)
	}
	err := c.Consume(queryRuntimeEvent("query", "replacement", "requested", 2))
	var mismatch *resumeIdentityMismatchError
	if !errors.As(err, &mismatch) {
		t.Fatalf("runtime error = %v, want typed resume identity mismatch", err)
	}

	if len(observed) != 1 || observed[0] != "requested" {
		t.Fatalf("registry observations = %v, want no observation from mismatched runtime", observed)
	}
	if got := len(c.snapshotRing()); got != 1 {
		t.Fatalf("retained frames = %d, want only QueryCreated", got)
	}
	if len(push.convo) != 1 || len(push.convo[0].GetItems()) != 1 {
		t.Fatalf("conversation pushes = %+v, want one typed identity failure", push.convo)
	}
	failure := push.convo[0].GetItems()[0].GetFailureCard()
	if failure == nil || failure.GetKind().GetSessionResumeFailed().GetDetail().GetIdentityMismatch().GetReplacementClaudeSessionId() != "replacement" || failure.GetKind().GetSessionResumeFailed().GetDetail().GetClaudeSessionId() != "requested" {
		t.Fatalf("identity failure = %+v", failure)
	}
}

// ---------------------------------------------------------------------------
// A ROTATION THE DAEMON ASKED FOR IS ADOPTED, NOT REFUSED.
//
// reviveSession{clear} submits `/clear` before the resumed query has ever
// reported an identity — the revival gate keeps every other prompt out until
// the cut lands — so the rotation the command causes IS the first observation.
// Reading that as a failed resume killed the controller for obeying the command
// it had just been given, and wedged the revival that owned it.
// ---------------------------------------------------------------------------

// THE DEFECT ITSELF. A clear was dispatched, so the rotated identity is the
// command working and must not be a mismatch.
func TestResumeIdentityTrackerAdoptsTheRotationAClearCaused(t *testing.T) {
	// Arrange.
	tracker := newResumeIdentityTracker()
	if _, _, err := tracker.observe(queryCreatedEvent("query", "requested", "requested", 1)); err != nil {
		t.Fatalf("created: %v", err)
	}
	tracker.noteContextClearDispatched()

	// Act.
	mismatch, adopted, err := tracker.observe(queryRuntimeEvent("query", "rotated", "requested", 2))

	// Assert.
	if err != nil || mismatch != nil {
		t.Fatalf("runtime after a dispatched /clear = (%+v, %v), want no mismatch", mismatch, err)
	}
	if adopted == nil || adopted.requestedVendorSessionID != "requested" || adopted.adoptedVendorSessionID != "rotated" {
		t.Fatalf("adoption = %+v, want the rotation reported as evidence", adopted)
	}
}

// THE DISCHARGE IS SPENT ON ONE ROTATION. Consenting to a clear's rotation is
// not consenting to every later identity change, so a second unexplained one
// meets the ordinary refusal again.
func TestResumeIdentityTrackerRefusesASecondRotationAfterAClear(t *testing.T) {
	// Arrange — a second query, resumed against the id the clear rotated to.
	tracker := newResumeIdentityTracker()
	if _, _, err := tracker.observe(queryCreatedEvent("first", "requested", "requested", 1)); err != nil {
		t.Fatalf("created first: %v", err)
	}
	tracker.noteContextClearDispatched()
	if _, adopted, err := tracker.observe(queryRuntimeEvent("first", "rotated", "requested", 2)); err != nil || adopted == nil {
		t.Fatalf("first rotation = (%+v, %v), want it adopted", adopted, err)
	}
	if _, _, err := tracker.observe(queryCreatedEvent("second", "rotated", "rotated", 3)); err != nil {
		t.Fatalf("created second: %v", err)
	}

	// Act.
	mismatch, adopted, err := tracker.observe(queryRuntimeEvent("second", "someone-else", "rotated", 4))

	// Assert.
	if err != nil || adopted != nil {
		t.Fatalf("second rotation = (%+v, %v), want no second adoption from one discharge", adopted, err)
	}
	if mismatch == nil || mismatch.observedVendorSessionID != "someone-else" {
		t.Fatalf("second rotation mismatch = %+v, want the ordinary fatal refusal", mismatch)
	}
}

// AND AN UNEXPLAINED ROTATION IS STILL FATAL with no clear anywhere in the
// picture, which is the coverage the discharge must not have weakened.
func TestResumeIdentityTrackerKeepsRefusingWithoutADispatchedClear(t *testing.T) {
	// Arrange.
	tracker := newResumeIdentityTracker()
	if _, _, err := tracker.observe(queryCreatedEvent("query", "requested", "requested", 1)); err != nil {
		t.Fatalf("created: %v", err)
	}

	// Act.
	mismatch, adopted, err := tracker.observe(queryRuntimeEvent("query", "rotated", "requested", 2))

	// Assert.
	if err != nil || adopted != nil {
		t.Fatalf("runtime with no dispatched clear = (%+v, %v), want no adoption", adopted, err)
	}
	if mismatch == nil {
		t.Fatal("an unexplained rotation was accepted; the resume commitment must still be fatal without a clear")
	}
}

// THE ADOPTED IDENTITY BECOMES THIS QUERY'S OWN, so a later observation of the
// rotated conversation is an ordinary repeat rather than a fresh mismatch.
func TestResumeIdentityTrackerKeepsTheAdoptedIdentity(t *testing.T) {
	// Arrange.
	tracker := newResumeIdentityTracker()
	if _, _, err := tracker.observe(queryCreatedEvent("query", "requested", "requested", 1)); err != nil {
		t.Fatalf("created: %v", err)
	}
	tracker.noteContextClearDispatched()
	if _, _, err := tracker.observe(queryRuntimeEvent("query", "rotated", "requested", 2)); err != nil {
		t.Fatalf("first rotation: %v", err)
	}

	// Act.
	mismatch, _, err := tracker.observe(queryRuntimeEvent("query", "rotated", "rotated", 3))

	// Assert.
	if err != nil || mismatch != nil {
		t.Fatalf("repeat observation of the adopted identity = (%+v, %v), want it accepted", mismatch, err)
	}
}

// THE CONSUMER SURVIVES IT. The adoption's whole point is that the controller's
// run loop is not killed, so Consume must return no error and the frame must
// still be applied.
func TestConsumerSurvivesTheRotationAClearCaused(t *testing.T) {
	// Arrange.
	push := &fakePusher{}
	c := newConsumer("ws", "agent-session", push, &fakeApplier{}, nil,
		newFakeClearCompactStore(), emptyTurnAccountingStore{}, t.Logf,
		nil, nil, nil, nil, nil)
	if err := c.Consume(queryCreatedEvent("query", "requested", "requested", 1)); err != nil {
		t.Fatal(err)
	}
	c.resumeIdentity.noteContextClearDispatched()

	// Act.
	err := c.Consume(queryRuntimeEvent("query", "rotated", "requested", 2))

	// Assert.
	if err != nil {
		t.Fatalf("Consume of the rotation a dispatched /clear caused = %v, want the controller to survive it", err)
	}
}
