package cachelapse

import (
	"context"
	"errors"
	"testing"
	"time"

	shimv1 "agentrepl/proto/shim/v1"

	"agent-repld/internal/dlog"
	"agent-repld/internal/wsm"
)

// withParked parks a conversation for theWorkspace whose cache was written
// CLOCKAGO, with the keep-alive request the fleet answers for it.
func withParked(h *harness, clockAgo time.Duration) *shimv1.SendKeepaliveParked {
	at := now.Add(-clockAgo)
	h.db.bypass[theWorkspace] = wsm.QueueBypass{ParkedVendorSessionID: "vendor-queue", ParkedClockAt: &at}
	request := &shimv1.SendKeepaliveParked{VendorSessionId: "vendor-queue"}
	h.parkedRequests = map[wsm.WorkspaceID]*shimv1.SendKeepaliveParked{theWorkspace: request}
	return request
}

// applyQueuedEdges waits for the edge an off-loop parked beat queues, then
// applies every queued edge as Run would.
func applyQueuedEdges(t *testing.T, h *harness) {
	t.Helper()
	<-h.keeper.wake
	h.keeper.mu.Lock()
	queued := h.keeper.queue
	h.keeper.queue = nil
	h.keeper.mu.Unlock()
	for _, e := range queued {
		h.keeper.apply(context.Background(), e)
	}
}

func TestAParkedConversationsBeatIsDueAtItsOwnClock(t *testing.T) {
	// Arrange: the running conversation's turn ended ten minutes ago, and the
	// parked one was last written fifty minutes ago.
	h := newHarness(t, beatable(visible(), 10*time.Minute))
	withParked(h, 50*time.Minute)

	// Act
	h.keeper.schedule(context.Background(), theWorkspace, "test")

	// Assert
	if got, want := h.keeper.beats[theWorkspace], now.Add(-50*time.Minute).Add(KeepaliveInterval); !got.Equal(want) {
		t.Fatalf("beat = %v, want the parked conversation's %v", got, want)
	}
}

func TestAParkedConversationOwesNoBeat(t *testing.T) {
	tests := []struct {
		name    string
		arrange func(h *harness)
	}{
		{name: "when its cache is known cold", arrange: func(h *harness) {
			standing := h.db.bypass[theWorkspace]
			standing.ParkedCold = true
			h.db.bypass[theWorkspace] = standing
		}},
		{name: "while one of its keep-alives is out", arrange: func(h *harness) { h.keeper.parkedFlying[theWorkspace] = true }},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t, beatable(visible(), 10*time.Minute))
			withParked(h, 50*time.Minute)
			tt.arrange(h)

			// Act
			h.keeper.schedule(context.Background(), theWorkspace, "test")

			// Assert
			if got, want := h.keeper.beats[theWorkspace], now.Add(-10*time.Minute).Add(KeepaliveInterval); !got.Equal(want) {
				t.Fatalf("beat = %v, want the running conversation's own %v", got, want)
			}
		})
	}
}

func TestAHiddenWorkspacesParkedConversationIsNotBeaten(t *testing.T) {
	// Arrange
	h := newHarness(t, hidden())
	withParked(h, 50*time.Minute)

	// Act
	h.keeper.schedule(context.Background(), theWorkspace, "test")

	// Assert
	if _, armed := h.keeper.beats[theWorkspace]; armed {
		t.Fatal("a hidden workspace's parked conversation was armed a beat")
	}
}

func TestADueBeatBeatsBothConversations(t *testing.T) {
	// Arrange
	h := dueHarness(t, sent())
	request := withParked(h, KeepaliveInterval+time.Minute)
	h.shim.parkedAnswer = sent()
	h.keeper.beats[theWorkspace] = now

	// Act
	h.keeper.beatsDue(context.Background())
	applyQueuedEdges(t, h)

	// Assert
	if h.shim.beats != 1 || len(h.shim.parked) != 1 || h.shim.parked[0] != request {
		t.Fatalf("beats = %d, parked = %v, want one of each with the fleet's request", h.shim.beats, h.shim.parked)
	}
}

func TestAParkedBeatOwedAloneSendsNoRunningConversationBeat(t *testing.T) {
	// Arrange: the running conversation has taken no turn since it was bound.
	h := newHarness(t, visible())
	withParked(h, KeepaliveInterval+time.Minute)
	h.shim.parkedAnswer = sent()
	h.keeper.beats[theWorkspace] = now

	// Act
	h.keeper.beatsDue(context.Background())
	applyQueuedEdges(t, h)

	// Assert
	if h.shim.beats != 0 || len(h.shim.parked) != 1 {
		t.Fatalf("beats = %d, parked = %d, want only the parked conversation beaten", h.shim.beats, len(h.shim.parked))
	}
}

func TestAParkedBeatsAnswer(t *testing.T) {
	lapsed := &shimv1.SendKeepaliveSuccess{Outcome: &shimv1.SendKeepaliveSuccess_Skipped{Skipped: &shimv1.SendKeepaliveSkipped{
		Reason: &shimv1.SendKeepaliveSkipped_CacheLapsed{CacheLapsed: &shimv1.SendKeepaliveSkippedCacheLapsed{LapsedAtMs: 1}},
	}}}
	tests := []struct {
		name   string
		answer *shimv1.SendKeepaliveSuccess
		err    error
		assert func(t *testing.T, h *harness)
	}{
		{name: "sent moves the parked clock", answer: sent(), assert: func(t *testing.T, h *harness) {
			if len(h.db.parkedKeepalives) != 1 || !h.db.parkedKeepalives[0].Equal(now) {
				t.Fatalf("parked keep-alives = %v, want one at %v", h.db.parkedKeepalives, now)
			}
		}},
		{name: "a lapsed cache is marked cold", answer: lapsed, assert: func(t *testing.T, h *harness) {
			if len(h.db.parkedCold) != 1 {
				t.Fatalf("parked cold marks = %v, want one", h.db.parkedCold)
			}
		}},
		{name: "a failed submission is asked again and logged", err: &BeatSubmitFailed{Detail: "overloaded"}, assert: func(t *testing.T, h *harness) {
			if got := h.keeper.parkedRetry[theWorkspace]; !got.Equal(now.Add(BeatRetry)) {
				t.Fatalf("retry = %v, want %v", got, now.Add(BeatRetry))
			}
			if !h.found(dlog.RecordQuery{Level: "error", Operation: opDrive, Message: "the parked conversation's keep-alive failed; it is asked again shortly"}) {
				t.Fatal("the failure was not logged at error")
			}
		}},
		{name: "an unreachable shim is asked again and logged", err: errors.New("socket closed"), assert: func(t *testing.T, h *harness) {
			if _, ok := h.keeper.parkedRetry[theWorkspace]; !ok {
				t.Fatal("no retry was armed")
			}
			if !h.found(dlog.RecordQuery{Level: "error", Operation: opDrive, Message: "could not ask the shim for the parked keep-alive; it is asked again shortly"}) {
				t.Fatal("the failure was not logged at error")
			}
		}},
		{name: "a shim with no session is asked again", err: ErrNoSession, assert: func(t *testing.T, h *harness) {
			if _, ok := h.keeper.parkedRetry[theWorkspace]; !ok {
				t.Fatal("no retry was armed")
			}
		}},
	}
	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Arrange
			h := newHarness(t, beatable(visible(), 10*time.Minute))
			withParked(h, 50*time.Minute)
			h.keeper.parkedFlying[theWorkspace] = true

			// Act
			h.keeper.parkedAnswered(context.Background(), theWorkspace, tt.answer, tt.err)

			// Assert
			if h.keeper.parkedFlying[theWorkspace] {
				t.Fatal("the parked keep-alive is still marked out")
			}
			tt.assert(t, h)
		})
	}
}

func TestASwitchForgetsTheSteppedOutConversationsSentClock(t *testing.T) {
	// Arrange
	h := newHarness(t, beatable(visible(), 10*time.Minute))
	h.keeper.sent[theWorkspace] = now

	// Act
	h.keeper.switched(context.Background(), theWorkspace)

	// Assert
	if _, kept := h.keeper.sent[theWorkspace]; kept {
		t.Fatal("the stepped-out conversation's sent clock still times the bound conversation's beat")
	}
}
