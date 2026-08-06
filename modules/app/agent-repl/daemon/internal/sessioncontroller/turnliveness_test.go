package sessioncontroller

import (
	"context"
	"fmt"
	"strings"
	"sync"
	"testing"

	"claude-repld/internal/ssm"
)

// The prompt queue's "is a turn in flight" test is the SSM's one turn-liveness
// derivation and nothing else. These cover the two ends of that: a derivation
// holding no live turn lets a prompt through, and a value nothing derived is
// refused rather than read as "no turn in flight".

// TestAPromptIsDeliveredOnceTheDerivationHoldsNoLiveTurn is the queue half of
// the superseded-turn wedge (see ssm/turnliveness_test.go for the store-side
// reproduction). The workspace held a turn that a supersede killed; once the
// derivation says nothing is in flight, the next prompt goes to the shim
// instead of queueing behind a boundary that will never arrive.
func TestAPromptIsDeliveredOnceTheDerivationHoldsNoLiveTurn(t *testing.T) {
	// Arrange — a named turn is in flight, so a prompt would queue.
	h := newQueueHarness(t, nil)
	d := h.controller()
	h.m.noteTurnLiveness(d, ssm.TurnLivenessFixture(d.workspace, []string{"turn-killed"}, false))
	if err := h.m.SubmitPrompt(context.Background(), d.workspace, "req-queued", "first", "", testPromptOrigin); err != nil {
		t.Fatalf("SubmitPrompt while a turn is in flight: %v", err)
	}
	if got := len(h.client.promptTexts()); got != 0 {
		t.Fatalf("prompts reaching the shim while a turn is in flight = %d, want 0 (queued)", got)
	}

	// Act — the derivation now holds no live turn, which is what a supersede's
	// durable end produces on the replacement session.
	h.m.noteTurnLiveness(d, ssm.TurnLivenessFixture(d.workspace, nil, false))
	if err := h.m.SubmitPrompt(context.Background(), d.workspace, "req-delivered", "second", "", testPromptOrigin); err != nil {
		t.Fatalf("SubmitPrompt after the turn was killed: %v", err)
	}

	// Assert — DELIVERED, not queued.
	delivered := h.client.promptTexts()
	if len(delivered) == 0 {
		t.Fatal("no prompt reached the shim: the queue is still holding behind a turn the derivation says is over")
	}
	joined := strings.Join(delivered, "|")
	if !strings.Contains(joined, "second") {
		t.Fatalf("prompts reaching the shim = %v, want the one submitted after the turn was killed", delivered)
	}
}

// TestAnUnderivedLivenessIsRefusedRatherThanReadAsIdle drives the invariant into
// violation. A zero ssm.TurnLiveness is not "no turn in flight" — it is not an
// answer — and reading it as idle is precisely the wedge: a green sidebar and
// prompts delivered into a live turn.
func TestAnUnderivedLivenessIsRefusedRatherThanReadAsIdle(t *testing.T) {
	// Arrange — a named turn stands in the record.
	var mu sync.Mutex
	var lines []string
	h := newQueueHarnessWithHolds(t, nil, nil, func(format string, args ...any) {
		mu.Lock()
		defer mu.Unlock()
		lines = append(lines, fmt.Sprintf(format, args...))
	})
	d := h.controller()
	h.m.noteTurnLiveness(d, ssm.TurnLivenessFixture(d.workspace, []string{"turn-live"}, false))

	// Act — a value nothing derived arrives.
	h.m.noteTurnLiveness(d, ssm.TurnLiveness{})

	// Assert — the record is untouched, so the live turn is not released...
	h.m.mu.Lock()
	after := d.turn
	h.m.mu.Unlock()
	if id, named := after.name(); !named || id != "turn-live" {
		t.Fatalf("turn record after an underived value = %s, want the live turn left exactly as it was", after)
	}
	// ...and the violation is recorded once, loudly, with the workspace and
	// session on it.
	mu.Lock()
	defer mu.Unlock()
	found := false
	for _, line := range lines {
		if strings.Contains(line, "INVARIANT VIOLATION") &&
			strings.Contains(line, "edge=turn_liveness") &&
			strings.Contains(line, "session=s1") {
			found = true
		}
	}
	if !found {
		t.Fatalf("no canonical invariant record for the underived value; log=%v", lines)
	}
}
