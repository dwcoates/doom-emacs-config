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
	if mismatch, err := tracker.observe(queryCreatedEvent("query", "requested", "requested", 1)); err != nil || mismatch != nil {
		t.Fatalf("created = (%+v, %v)", mismatch, err)
	}
	mismatch, err := tracker.observe(queryRuntimeEvent("query", "replacement", "requested", 2))
	if err != nil || mismatch == nil || mismatch.requestedVendorSessionID != "requested" || mismatch.observedVendorSessionID != "replacement" {
		t.Fatalf("runtime = (%+v, %v)", mismatch, err)
	}
	if repeated, err := tracker.observe(queryRuntimeEvent("query", "replacement", "requested", 2)); err != nil || repeated == nil {
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
			if mismatch, err := tracker.observe(queryCreatedEvent("query", tc.requested, tc.requested, 1)); err != nil || mismatch != nil {
				t.Fatalf("created = (%+v, %v)", mismatch, err)
			}
			if mismatch, err := tracker.observe(queryRuntimeEvent("query", tc.observed, tc.observed, 2)); err != nil || mismatch != nil {
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
