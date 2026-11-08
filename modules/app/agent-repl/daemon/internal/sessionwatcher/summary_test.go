package sessionwatcher

import (
	"sync"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/ids"
	"agent-repld/internal/wsm"
)

// recordingTitleSink records every TitleSink trigger it receives.
type recordingTitleSink struct {
	mu    sync.Mutex
	calls []string
}

func (t *recordingTitleSink) OnSessionStarted(ids.WorkspaceID) { t.record("OnSessionStarted") }
func (t *recordingTitleSink) OnPrompt(ids.WorkspaceID)         { t.record("OnPrompt") }
func (t *recordingTitleSink) OnTurnEnded(ids.WorkspaceID)      { t.record("OnTurnEnded") }
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

func TestTheVendorsTitleTellsTheSynthesizerNothing(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()
	ts := h.withTitle()

	// Act.
	h.routeNow(func(w *watcher) { w.routeSessionUpdateLocked(titleUpdate()) })

	// Assert: the daemon's own summary outranks the vendor's, so it stops nothing.
	if len(ts.calls) != 0 {
		t.Fatalf("the vendor's title reached the synthesizer (calls = %v), want none", ts.calls)
	}
}

func TestAMainPromptTriggersSynthesis(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()
	ts := h.withTitle()

	// Act.
	h.routeNow(func(w *watcher) {
		w.routePromptLocked(&agentWatch{}, promptEntry("ptr-1", "turn-1", "agent-1").GetEntry().GetUserPrompt(), nil)
	})

	// Assert.
	if !ts.has("OnPrompt") {
		t.Fatalf("a main-agent prompt did not trigger synthesis (calls = %v)", ts.calls)
	}
}

func TestASubagentPromptDoesNotTriggerSynthesis(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()
	ts := h.withTitle()

	// Act.
	h.routeNow(func(w *watcher) {
		w.routePromptLocked(&agentWatch{id: agentID("sub-1")}, promptEntry("ptr-1", "turn-1", "sub-1").GetEntry().GetUserPrompt(), nil)
	})

	// Assert: a subagent's commission is not the user's prompt.
	if ts.has("OnPrompt") {
		t.Fatalf("a subagent prompt triggered synthesis (calls = %v)", ts.calls)
	}
}

func TestACompactionDoesNotResetTheSynthesizer(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()
	ts := h.withTitle()

	// Act.
	h.routeNow(func(w *watcher) {
		w.routeUpdateLocked(&conversationv1.AgentId{Value: "agent-1"}, &conversationv1.AgentUpdate{
			Update: &conversationv1.AgentUpdate_ContextCut{ContextCut: compactedCut()},
		}, nil, nil, nil)
	})

	// Assert: a compaction cuts no prompt from the digest.
	if ts.has("OnContextReset") {
		t.Fatalf("a /compact reset the synthesizer (calls = %v)", ts.calls)
	}
}

func TestAClearResetsTheSynthesizer(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()
	ts := h.withTitle()

	// Act.
	h.routeNow(func(w *watcher) {
		w.routeUpdateLocked(&conversationv1.AgentId{Value: "agent-1"}, clearedCutUpdate(), nil, nil, nil)
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
