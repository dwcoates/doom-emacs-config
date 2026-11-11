package footer

import (
	"sync"
	"testing"

	"agent-repld/internal/ids"
	"agent-repld/internal/viewquiet"
)

// quietHarness is a connected footer behind a quiet gate, recording every
// status edge it tells.
func quietHarness(t *testing.T) (*harness, *viewquiet.Gate, func() []ids.WorkspaceID) {
	t.Helper()
	gate := viewquiet.New(nil)
	var mu sync.Mutex
	var told []ids.WorkspaceID
	h := newHarness(t, WithQuiet(gate), WithStatusChanged(func(ws ids.WorkspaceID) {
		mu.Lock()
		defer mu.Unlock()
		told = append(told, ws)
	}))
	connected(h)
	mu.Lock()
	told = nil
	mu.Unlock()
	return h, gate, func() []ids.WorkspaceID {
		mu.Lock()
		defer mu.Unlock()
		return append([]ids.WorkspaceID(nil), told...)
	}
}

func TestAQuietFooterPublishesNothingButAnswersTheLiveStatus(t *testing.T) {
	// Arrange
	h, gate, told := quietHarness(t)
	before := h.status(t)
	gate.Set(true)

	// Act
	h.r.SetTurn(testWS, &TurnStarted{At: h.clock.Now(), Act: ActPrompt})

	// Assert
	if got := h.status(t); got != before {
		t.Fatalf("published status = %q, want the pre-quiet %q: a quiet footer publishes nothing", got, before)
	}
	if got := statusName(h.r.Status(testWS)); got == before {
		t.Fatalf("Status() = %q, want the live status the turn moved to", got)
	}
	if got := told(); len(got) != 1 || got[0] != testWS {
		t.Fatalf("status edges told = %v, want the one move", got)
	}
	if h.r.TurnClock(testWS).GetTurnStartedAtMs() == 0 {
		t.Fatal("TurnClock() has no start: the turn clock was not resolved while quiet")
	}
}

func TestLeavingTheQuietStatePublishesTheCurrentFooter(t *testing.T) {
	// Arrange
	h, gate, _ := quietHarness(t)
	gate.Set(true)
	h.r.SetTurn(testWS, &TurnStarted{At: h.clock.Now(), Act: ActPrompt})
	live := statusName(h.r.Status(testWS))

	// Act
	gate.Set(false)

	// Assert
	view := h.view(t)
	if got := statusName(view.GetStrip().GetStatus()); got != live {
		t.Fatalf("published status = %q, want the current %q", got, live)
	}
	if view.GetExpanded() == nil {
		t.Fatal("the resync published a facts-only view, want the whole view")
	}
}
