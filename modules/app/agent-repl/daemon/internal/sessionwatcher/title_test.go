package sessionwatcher

import (
	"sync"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"

	"claude-repld/internal/ids"
	"claude-repld/internal/wsm"
)

// recordingTitleSink records every TitleSink trigger it receives.
type recordingTitleSink struct {
	mu    sync.Mutex
	calls []string
}

func (t *recordingTitleSink) OnSessionStarted(ids.WorkspaceID) { t.record("OnSessionStarted") }
func (t *recordingTitleSink) OnTurnEnded(ids.WorkspaceID)      { t.record("OnTurnEnded") }
func (t *recordingTitleSink) OnVendorTitle(ids.WorkspaceID)    { t.record("OnVendorTitle") }
func (t *recordingTitleSink) OnContextReset(ids.WorkspaceID)   { t.record("OnContextReset") }

func (t *recordingTitleSink) record(name string) {
	t.mu.Lock()
	defer t.mu.Unlock()
	t.calls = append(t.calls, name)
}

func (t *recordingTitleSink) has(name string) bool {
	t.mu.Lock()
	defer t.mu.Unlock()
	for _, c := range t.calls {
		if c == name {
			return true
		}
	}
	return false
}

// clearedCutUpdate is an agent-plane update carrying a /clear context cut.
func clearedCutUpdate() *conversationv1.AgentUpdate {
	return &conversationv1.AgentUpdate{
		Update: &conversationv1.AgentUpdate_ContextCut{
			ContextCut: &conversationv1.ContextCut{
				Cut: &conversationv1.ContextCut_Cleared{Cleared: &conversationv1.ContextCleared{}},
			},
		},
	}
}

func TestTheVendorsTitleStopsSynthesis(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()
	ts := h.withTitle()

	// Act.
	h.routeNow(func(w *watcher) { w.routeSessionUpdateLocked(titleUpdate()) })

	// Assert.
	if !ts.has("OnVendorTitle") {
		t.Fatalf("the vendor's title did not stop synthesis (calls = %v)", ts.calls)
	}
}

func TestAClearResetsTheSynthesizer(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()
	ts := h.withTitle()

	// Act.
	h.routeNow(func(w *watcher) {
		w.routeUpdateLocked(&conversationv1.AgentId{Value: "agent-1"}, clearedCutUpdate(), nil, nil)
	})

	// Assert.
	if !ts.has("OnContextReset") {
		t.Fatalf("a /clear did not reset the synthesizer (calls = %v)", ts.calls)
	}
}

func TestATurnEndTriggersSynthesis(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()
	ts := h.withTitle()

	// Act — a pending turn end flushes to both the lifecycle and title sinks.
	h.routeNow(func(w *watcher) {
		w.pendingTurnEnds = append(w.pendingTurnEnds, endedTurn{turn: ids.TurnID("t1"), how: wsm.CloseCompleted})
	})

	// Assert.
	if !ts.has("OnTurnEnded") {
		t.Fatalf("a turn end did not trigger synthesis (calls = %v)", ts.calls)
	}
}

func TestASessionStartTriggersSynthesis(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()
	ts := h.withTitle()

	// Act — a resumed/adopted session names itself; the synthesizer is told.
	h.routeNow(func(w *watcher) { w.applySessionStartedLocked(sessionStarted("")) })

	// Assert.
	if !ts.has("OnSessionStarted") {
		t.Fatalf("a session start did not trigger synthesis (calls = %v)", ts.calls)
	}
}
