package sessioncontroller

import (
	"fmt"
	"strings"
	"testing"

	corev1 "agentrepl/proto/agentshim/core/v1"
	datav1 "agentrepl/proto/agentshim/data/v1"

	"claude-repld/internal/registry"

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
	h.controller().consumer.Consume(systemInitEvent(t, 1, "claude-fable-5"))

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
	h.controller().consumer.Consume(systemInitEvent(t, 1, "<synthetic>"))

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
	h.controller().consumer.Consume(systemInitEvent(t, 1, ""))

	// Assert.
	if got := h.observedModels(); len(got) != 0 {
		t.Fatalf("observed models = %q, want an empty report ignored", got)
	}
}

// --- the ordering token every report must carry -----------------------------
//
// The record's two writers used to race. Every observation now states WHEN it
// was true; the admission rule those tokens feed lives in the production
// registrar (server.RegistryRegistrar), and these cover what the daemon MINTS.

// observedTokens returns the ordering tokens the harness's registrar was told.
func (h *queueHarness) observedTokens() []registry.ModelObservation {
	h.reg.mu.Lock()
	defer h.reg.mu.Unlock()
	return append([]registry.ModelObservation(nil), h.reg.observedTokens...)
}

func TestAStreamReportedModelIsTokenedWithTheSeqItRodeInOn(t *testing.T) {
	// Arrange — the seq is what orders a SystemInit against a shim
	// confirmation, so a report that lost it could not be ordered at all.
	h := newQueueHarness(t, nil)

	// Act.
	h.controller().consumer.Consume(systemInitEvent(t, 7, "claude-fable-5"))

	// Assert.
	got := h.observedTokens()
	if len(got) != 1 || got[0].StreamSeq != 7 {
		t.Fatalf("observation tokens = %v, want one carrying stream seq 7", got)
	}
}

func TestAStreamReportedModelIsTokenedWithItsControllerGeneration(t *testing.T) {
	// Arrange — a report left in flight by a RETIRED controller must lose to
	// its replacement's, which only the generation ordinal can decide.
	h := newQueueHarness(t, nil)
	d := h.controller()

	// Act.
	d.consumer.Consume(systemInitEvent(t, 7, "claude-fable-5"))

	// Assert.
	got := h.observedTokens()
	if len(got) != 1 || got[0].GenOrdinal != d.genOrdinal || got[0].Generation != d.generationID {
		t.Fatalf("observation tokens = %v, want the live controller's generation %s/%d", got, d.generationID, d.genOrdinal)
	}
}

func TestAConfirmedModelIsTokenedWithTheConsumedSeqWatermark(t *testing.T) {
	// Arrange — a confirmation is ground truth AS OF everything already
	// consumed, so it must outrank every init at or below the mark. Consuming
	// seq 9 first is what puts the mark there.
	h := newQueueHarness(t, nil)
	h.controller().consumer.Consume(systemInitEvent(t, 9, "claude-sonnet-5"))

	// Act.
	if err := h.submitAs("r1", "/model opus"); err != nil {
		t.Fatalf("submit: %v", err)
	}

	// Assert.
	got := h.observedTokens()
	if len(got) != 2 || got[1].StreamSeq != 9 {
		t.Fatalf("observation tokens = %v, want the confirmation marked as of consumed seq 9", got)
	}
}

func TestAnUntokenedModelObservationNeverReachesTheRegistrar(t *testing.T) {
	// Arrange — the funnel refuses a report that orders against nothing rather
	// than passing it on, because admitting it restores last-writer-wins for
	// every observation that follows it.
	var logged []string
	h := newQueueHarnessWithPusher(t, nil, nil, func(f string, a ...any) {
		logged = append(logged, fmt.Sprintf(f, a...))
	})

	// Act.
	h.m.persistObservedModel("s1", "claude-opus-5", registry.ModelObservation{})

	// Assert.
	if got := h.observedModels(); len(got) != 0 {
		t.Fatalf("observed models = %q, want the untokened report refused", got)
	}
	var loud bool
	for _, line := range logged {
		if strings.Contains(line, "untokened_observation") {
			loud = true
		}
	}
	if !loud {
		t.Fatalf("log = %v, want the untokened refusal recorded", logged)
	}
}
