package sessionwatcher

import (
	"sync"
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"

	"agent-repld/internal/ids"
)

// recordingSummarySink records every /clear the summary keeper is told of.
type recordingSummarySink struct {
	mu     sync.Mutex
	resets []ids.WorkspaceID
}

func (t *recordingSummarySink) OnContextReset(ws ids.WorkspaceID) {
	t.mu.Lock()
	defer t.mu.Unlock()
	t.resets = append(t.resets, ws)
}

// reset reports whether any /clear reached the sink.
func (t *recordingSummarySink) reset() bool {
	t.mu.Lock()
	defer t.mu.Unlock()
	return len(t.resets) > 0
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

func TestACompactionLeavesTheSummaryStanding(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()
	ts := h.withSummary()

	// Act.
	h.routeNow(func(w *watcher) {
		w.routeUpdateLocked(&conversationv1.AgentId{Value: "agent-1"}, &conversationv1.AgentUpdate{
			Update: &conversationv1.AgentUpdate_ContextCut{ContextCut: compactedCut()},
		}, nil, nil, nil)
	})

	// Assert: a compaction keeps the conversation the summary describes.
	if ts.reset() {
		t.Fatalf("a /compact reset the summary (resets = %v)", ts.resets)
	}
}

func TestAClearTellsTheSummaryKeeperItsWorkspace(t *testing.T) {
	// Arrange.
	h := newHarness(t, Session{Started: sessionStarted("")})
	h.quiet()
	ts := h.withSummary()

	// Act.
	h.routeNow(func(w *watcher) {
		w.routeUpdateLocked(&conversationv1.AgentId{Value: "agent-1"}, clearedCutUpdate(), nil, nil, nil)
	})

	// Assert.
	if len(ts.resets) != 1 || ts.resets[0] != h.w.ws {
		t.Fatalf("resets = %v, want exactly the watched workspace %q", ts.resets, h.w.ws)
	}
}
