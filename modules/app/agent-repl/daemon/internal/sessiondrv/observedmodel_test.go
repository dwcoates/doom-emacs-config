package sessiondrv

import (
	"testing"

	corev1 "agentrepl/proto/agentshim/core/v1"
	datav1 "agentrepl/proto/agentshim/data/v1"

	"google.golang.org/protobuf/types/known/anypb"
)

// The model a LIVE session reports is the only one a respawn should be pinned
// to. The record used to freeze at create, so a session that changed model
// mid-life was relaunched as the original after every hibernation.

// systemInitEvent is a vendor stream-plane SystemInit, which the SDK re-emits on
// every submit rather than only at session start.
func systemInitEvent(t *testing.T, seq uint64, model string) *corev1.Event {
	t.Helper()
	a, err := anypb.New(&datav1.ClaudeStreamMessage{
		Msg: &datav1.ClaudeStreamMessage_SystemInit{SystemInit: &datav1.SystemInit{Model: model}},
	})
	if err != nil {
		t.Fatalf("anypb.New: %v", err)
	}
	return &corev1.Event{SessionId: "vendor-uuid", Seq: seq, Payload: &corev1.Event_Vendor{Vendor: a}}
}

// observedModels returns the models the harness's registrar was told about.
func (h *queueHarness) observedModels() []string {
	h.reg.mu.Lock()
	defer h.reg.mu.Unlock()
	return append([]string(nil), h.reg.observedModels...)
}

func TestAReportedModelIsPersisted(t *testing.T) {
	// Arrange — a live session whose init announces the model it is running.
	h := newQueueHarness(t, nil)

	// Act.
	h.driver().consumer.Consume(systemInitEvent(t, 1, "claude-fable-5"))

	// Assert: the record follows the SESSION, not the create request.
	got := h.observedModels()
	if len(got) != 1 || got[0] != "claude-fable-5" {
		t.Fatalf("observed models = %q, want the reported model persisted", got)
	}
}

func TestTheCLIPlaceholderIsNeverPersistedAsAModel(t *testing.T) {
	// Arrange — the CLI reports its placeholder when it has no real model to
	// name, and writing it down poisons the record with a value nothing can be
	// spawned under.
	h := newQueueHarness(t, nil)

	// Act.
	h.driver().consumer.Consume(systemInitEvent(t, 1, "<synthetic>"))

	// Assert.
	if got := h.observedModels(); len(got) != 0 {
		t.Fatalf("observed models = %q, want the placeholder refused", got)
	}
}

func TestAnEmptyReportedModelWritesNothing(t *testing.T) {
	// Arrange — an init that names no model says nothing about the session's
	// model, and must not blank a record that already holds a real one.
	h := newQueueHarness(t, nil)

	// Act.
	h.driver().consumer.Consume(systemInitEvent(t, 1, ""))

	// Assert.
	if got := h.observedModels(); len(got) != 0 {
		t.Fatalf("observed models = %q, want an empty report ignored", got)
	}
}
