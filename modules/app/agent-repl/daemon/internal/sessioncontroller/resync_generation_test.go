package sessioncontroller

import (
	"context"
	"errors"
	"fmt"
	"strings"
	"sync"
	"testing"

	corev1 "agentrepl/proto/agentshim/core/v1"
	frontendv1 "agentrepl/proto/agentshim/frontend/v1"
	"claude-repld/internal/errclass"
	"claude-repld/internal/ssm"
	"claude-repld/internal/storehistory"
)

type gatedDurableHistory struct {
	mu      sync.Mutex
	calls   int
	entered chan struct{}
	release chan struct{}
	once    sync.Once
}

func (h *gatedDurableHistory) ReplayHistory(_ context.Context, _, _ string, _, _ uint64, _ uint32, _ func(*corev1.Event)) (storehistory.Result, error) {
	h.mu.Lock()
	h.calls++
	h.mu.Unlock()
	h.once.Do(func() { close(h.entered) })
	<-h.release
	return storehistory.Result{}, nil
}

func (h *gatedDurableHistory) callCount() int {
	h.mu.Lock()
	defer h.mu.Unlock()
	return h.calls
}

func TestResyncForGenerationRejectsAnOldClientBeforeReplay(t *testing.T) {
	client := &replayClient{}
	h := newRepullHarness(t, client)
	d := h.controller(t)

	err := h.m.ResyncForFence("ws", ssm.Fence(d.sessionID, d.generationID+"-retired"), 0)
	if !errors.Is(err, errclass.ErrSessionSuperseded) {
		t.Fatalf("ResyncForGeneration error = %v, want session superseded", err)
	}
	if got := client.callCount(); got != 0 {
		t.Fatalf("stale resync started %d shim replay(s), want none", got)
	}
}

func TestResyncForGenerationRejectsOldClientAcrossLifecycleBoundaries(t *testing.T) {
	tests := []struct {
		name   string
		mutate func(*repullHarness, *sessionController)
	}{
		{"detach_in_progress", func(h *repullHarness, _ *sessionController) {
			h.m.mu.Lock()
			if h.m.hibernating == nil {
				h.m.hibernating = map[string]bool{}
			}
			h.m.hibernating["ws"] = true
			h.m.mu.Unlock()
		}},
		{"detach", func(h *repullHarness, d *sessionController) {
			h.m.mu.Lock()
			delete(h.m.byWS, "ws")
			h.m.mu.Unlock()
			h.applier.setCurrent("ws", &frontendv1.WorkspaceState{Workspace: "ws", SessionId: d.sessionID, ControllerGenerationId: ""})
		}},
		{"supersession", func(h *repullHarness, d *sessionController) {
			h.m.mu.Lock()
			d.sessionID, d.generationID = "s-successor", "generation-successor"
			h.m.mu.Unlock()
		}},
		{"replacement_after_reconnect", func(h *repullHarness, d *sessionController) {
			h.m.mu.Lock()
			d.generationID += "-replacement"
			h.m.mu.Unlock()
		}},
		{"delayed_old_client", func(h *repullHarness, d *sessionController) {
			h.m.mu.Lock()
			d.sessionID, d.generationID = "s-new", "generation-new"
			h.m.mu.Unlock()
		}},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			client := &replayClient{}
			history := &durableHistorySpy{}
			h := newRepullHarnessWithStore(t, client, history, nil)
			d := h.controller(t)
			oldSession, oldGeneration := d.sessionID, d.generationID
			tc.mutate(h, d)
			err := h.m.ResyncForFence("ws", ssm.Fence(oldSession, oldGeneration), 0)
			if !errors.Is(err, errclass.ErrSessionSuperseded) {
				t.Fatalf("error = %v, want superseded", err)
			}
			if got := client.callCount(); got != 0 {
				t.Fatalf("stale %s started %d shim replay(s), want 0", tc.name, got)
			}
			if got := len(history.replays()); got != 0 {
				t.Fatalf("stale %s started %d durable replay(s), want 0", tc.name, got)
			}
		})
	}
}

func TestResyncForGenerationAcceptsReconnectWithinTheSameControllerGeneration(t *testing.T) {
	client := &replayClient{}
	h := newRepullHarness(t, client)
	d := h.controller(t)
	sessionID, generationID := d.sessionID, d.generationID

	h.m.onLinkLostForGeneration("ws", sessionID, generationID, errors.New("test link loss"))
	if retiring := h.m.onConnectedForGeneration("ws", sessionID, generationID, &corev1.ShimHello{SessionId: sessionID}); retiring {
		t.Fatal("same-generation reconnect retired the live controller")
	}
	if err := h.m.ResyncForFence("ws", ssm.Fence(sessionID, generationID), 0); err != nil {
		t.Fatalf("same-generation reconnect resync: %v", err)
	}
	if got := client.callCount(); got != 1 {
		t.Fatalf("same-generation reconnect started %d shim replay(s), want 1", got)
	}
}

func TestDurableResyncAdmissionExcludesControllerInsertionUntilReplayEnds(t *testing.T) {
	history := &gatedDurableHistory{entered: make(chan struct{}), release: make(chan struct{})}
	h := newDurableHarness(t, history)
	// AN UNGENERATED WORKSPACE PUBLISHES AN ABSENT FENCE (ssm.compositeWorkspaceState),
	// so that is what this state carries and that is what the client echoes.
	// `Fence("s1", "")` would be a token the mint never produces and no client
	// was ever shown.
	h.applier.setCurrent("ws", &frontendv1.WorkspaceState{
		Workspace: "ws", SessionId: "s1", ControllerGenerationId: "", Fence: "",
	})
	done := make(chan error, 1)
	go func() { done <- h.m.ResyncForFence("ws", "", 0) }()
	// Selecting on the resync's own return keeps a REFUSAL a test failure rather
	// than a hang: an unconditional receive here wedges the whole package.
	select {
	case <-history.entered:
	case err := <-done:
		t.Fatalf("durable resync returned %v without ever entering the history read", err)
	}

	if h.m.mu.TryLock() {
		h.m.mu.Unlock()
		t.Fatal("controller insertion lock was available during durable replay")
	}
	close(history.release)
	if err := <-done; err != nil {
		t.Fatalf("durable resync: %v", err)
	}
	if got := history.callCount(); got != 1 {
		t.Fatalf("durable replay calls = %d, want 1", got)
	}
	if !h.m.mu.TryLock() {
		t.Fatal("controller insertion lock remained held after durable replay")
	}
	h.m.mu.Unlock()
}

func TestResyncForGenerationAcceptsTheCurrentController(t *testing.T) {
	client := &replayClient{}
	h := newRepullHarness(t, client)
	d := h.controller(t)

	if err := h.m.ResyncForFence("ws", ssm.Fence(d.sessionID, d.generationID), 0); err != nil {
		t.Fatalf("ResyncForGeneration: %v", err)
	}
	if got := client.callCount(); got != 1 {
		t.Fatalf("current resync started %d shim replay(s), want 1", got)
	}
}

func TestResyncForGenerationAcceptsAStaleSessionWithinTheCurrentGeneration(t *testing.T) {
	client := &replayClient{}
	h := newRepullHarness(t, client)
	d := h.controller(t)

	err := h.m.ResyncForFence("ws", ssm.Fence(d.sessionID+"-superseded", d.generationID), 0)

	if err != nil {
		t.Fatalf("same-generation stale-session resync: %v", err)
	}
	if got := client.callCount(); got != 1 {
		t.Fatalf("same-generation stale-session resync started %d shim replay(s), want 1", got)
	}
}

func TestResyncForGenerationRejectsAStaleSessionInAnotherGeneration(t *testing.T) {
	client := &replayClient{}
	h := newRepullHarness(t, client)
	d := h.controller(t)

	err := h.m.ResyncForFence("ws", ssm.Fence(d.sessionID+"-superseded", d.generationID+"-retired"), 0)

	if !errors.Is(err, errclass.ErrSessionSuperseded) {
		t.Fatalf("error = %v, want superseded", err)
	}
	if got := client.callCount(); got != 0 {
		t.Fatalf("cross-generation stale resync started %d shim replay(s), want 0", got)
	}
}

func TestResyncForGenerationRejectsAStaleSessionWithNoGeneration(t *testing.T) {
	client := &replayClient{}
	h := newRepullHarness(t, client)
	d := h.controller(t)
	h.m.mu.Lock()
	d.generationID = ""
	h.m.mu.Unlock()

	err := h.m.ResyncForFence("ws", ssm.Fence(d.sessionID+"-superseded", ""), 0)

	if !errors.Is(err, errclass.ErrSessionSuperseded) {
		t.Fatalf("error = %v, want superseded", err)
	}
	if got := client.callCount(); got != 0 {
		t.Fatalf("generationless stale resync started %d shim replay(s), want 0", got)
	}
}

func TestResyncForGenerationLogsTheSessionRebindingDecision(t *testing.T) {
	var lines []string
	client := &replayClient{}
	h := newRepullHarnessWithLog(t, client, func(format string, args ...any) {
		lines = append(lines, fmt.Sprintf(format, args...))
	})
	d := h.controller(t)

	_ = h.m.ResyncForFence("ws", ssm.Fence("retired-session", d.generationID), 0)

	for _, line := range lines {
		// The record names both identities as FENCES, because the fence is what
		// the client sent and what the ladder compared. Each one still carries
		// its session half, so the diagnostic loses nothing by naming the token
		// the decision was actually made on.
		if strings.Contains(line, "decision=current_generation_session_rebound") &&
			strings.Contains(line, "request_fence=\""+ssm.Fence("retired-session", d.generationID)+"\"") &&
			strings.Contains(line, "live_fence=\""+ssm.Fence(d.sessionID, d.generationID)+"\"") {
			return
		}
	}
	t.Fatalf("missing session-rebound acceptance diagnostic: %v", lines)
}

func TestResyncForGenerationLogsBothIdentitiesAndReplaySource(t *testing.T) {
	var lines []string
	client := &replayClient{}
	h := newRepullHarnessWithLog(t, client, func(format string, args ...any) {
		lines = append(lines, fmt.Sprintf(format, args...))
	})
	d := h.controller(t)

	_ = h.m.ResyncForFence("ws", ssm.Fence("retired-session", "retired-generation"), 0)
	for _, line := range lines {
		if strings.Contains(line, "decision=superseded") &&
			strings.Contains(line, "request_fence=\""+ssm.Fence("retired-session", "retired-generation")+"\"") &&
			strings.Contains(line, "live_fence=\""+ssm.Fence(d.sessionID, d.generationID)+"\"") &&
			strings.Contains(line, "replay_source=\"live_controller\"") {
			return
		}
	}
	t.Fatalf("missing classified stale-resync diagnostic: %v", lines)
}

func TestResyncForGenerationLogsHibernationRevocationBeforeReplay(t *testing.T) {
	var lines []string
	client := &replayClient{}
	h := newRepullHarnessWithLog(t, client, func(format string, args ...any) {
		lines = append(lines, fmt.Sprintf(format, args...))
	})
	d := h.controller(t)
	h.m.mu.Lock()
	if h.m.hibernating == nil {
		h.m.hibernating = map[string]bool{}
	}
	h.m.hibernating["ws"] = true
	h.m.mu.Unlock()

	err := h.m.ResyncForFence("ws", ssm.Fence(d.sessionID, d.generationID), 9)
	if !errors.Is(err, errclass.ErrSessionSuperseded) {
		t.Fatalf("error = %v, want superseded", err)
	}
	if client.callCount() != 0 {
		t.Fatalf("hibernation-revoked resync started shim replay")
	}
	for _, line := range lines {
		if strings.Contains(line, `replay_source="hibernation_transition"`) &&
			strings.Contains(line, `rejection_cause="eligibility_revoked"`) &&
			strings.Contains(line, "from_seq=9") {
			return
		}
	}
	t.Fatalf("missing hibernation revocation diagnostic: %v", lines)
}

// TestResyncForGenerationAcceptsAnIdentitylessRequestAgainstALiveController
// covers the COMPATIBILITY PATH: a client holding no fence at all carries
// neither a session nor a generation, so there is no identity to be stale
// about and nothing to refuse.
func TestResyncForGenerationAcceptsAnIdentitylessRequestAgainstALiveController(t *testing.T) {
	// Arrange
	client := &replayClient{}
	h := newRepullHarness(t, client)
	h.controller(t)

	// Act — NO fence at all. `Fence("", "")` would be the separator alone: a
	// non-empty token, and therefore a claim rather than the absence of one.
	err := h.m.ResyncForFence("ws", "", 0)

	// Assert
	if err != nil {
		t.Fatalf("identity-less resync against a live controller: %v", err)
	}
}

// TestResyncForGenerationStillRejectsAnAdoptedEmptyGenerationFence is the OTHER
// half of the wildcard's boundary: a request carrying a session with an empty
// generation DID adopt a fence, so it is compared and refused exactly as
// before. Only a request with no identity whatsoever is a wildcard.
func TestResyncForGenerationStillRejectsAnAdoptedEmptyGenerationFence(t *testing.T) {
	// Arrange
	client := &replayClient{}
	h := newRepullHarness(t, client)
	d := h.controller(t)

	// Act
	err := h.m.ResyncForFence("ws", ssm.Fence(d.sessionID, ""), 0)

	// Assert
	if !errors.Is(err, errclass.ErrSessionSuperseded) {
		t.Fatalf("error = %v, want superseded", err)
	}
}
