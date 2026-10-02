package topbar

import (
	"testing"

	conversationv1 "agentrepl/proto/conversation/v1"
)

func TestTheWindowFillIsTheFigureOverTheVendorsWindow(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.ready(t)

	// Act.
	h.r.OnSessionUpdate(testWS, contextUsage(50_000, 200_000, 25, "claude-opus-5"))

	// Assert.
	if got := h.view(t).GetContext().GetWindowFill(); got != 0.25 {
		t.Errorf("window_fill = %v, want 0.25", got)
	}
}

func TestTheWindowFillAssumesAMillionTokensWhenTheVendorStatesNoWindow(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.ready(t)

	// Act.
	h.r.OnSessionUpdate(testWS, contextUsage(250_000, 0, 0, "claude-opus-5"))

	// Assert.
	if got := h.view(t).GetContext().GetWindowFill(); got != 0.25 {
		t.Errorf("window_fill = %v, want 0.25 of the assumed 1,000,000", got)
	}
}

func TestTheWindowFillIsClampedToOne(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.ready(t)

	// Act.
	h.r.OnSessionUpdate(testWS, contextUsage(300_000, 200_000, 150, "claude-opus-5"))

	// Assert.
	if got := h.view(t).GetContext().GetWindowFill(); got != 1 {
		t.Errorf("window_fill = %v, want 1", got)
	}
}

func TestTheWindowFillIsZeroWithNoContext(t *testing.T) {
	// Arrange.
	h := newHarness(t)

	// Act.
	h.r.SetNaming(testWS, Naming{Title: "w", ConfigDir: "/root"})
	h.r.SetAccount(testWS, testAccount("dev@example.com"))

	// Assert.
	if got := h.view(t).GetContext().GetWindowFill(); got != 0 {
		t.Errorf("window_fill = %v, want 0", got)
	}
}

func TestTheColdGatesFigureIsMeasuredAgainstTheAssumedWindow(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.r.SetNaming(testWS, Naming{Title: "w", ConfigDir: "/root"})
	h.r.SetAccount(testWS, testAccount("dev@example.com"))

	// Act.
	h.r.SetColdGate(testWS, ColdGate{Standing: true, ContextTokens: 100_000})

	// Assert.
	if got := h.view(t).GetContext().GetWindowFill(); got != 0.1 {
		t.Errorf("window_fill = %v, want 0.1", got)
	}
}

func TestTheWindowsSourceIsRecordedWhenItChanges(t *testing.T) {
	// Arrange.
	h := newHarness(t)
	h.r.SetNaming(testWS, Naming{Title: "w", ConfigDir: "/root"})
	h.r.SetAccount(testWS, testAccount("dev@example.com"))

	// Act.
	h.r.OnSessionStarted(testWS, sessionStarted("vend-1", "claude-opus-5"))
	h.r.OnSessionUpdate(testWS, &conversationv1.SessionUpdate{
		Update: &conversationv1.SessionUpdate_ContextUsage{
			ContextUsage: &conversationv1.SessionContextUsage{TotalTokens: 1, MaxTokens: 200_000},
		},
	})

	// Assert.
	var sources []any
	for _, record := range h.log.Records() {
		if record.Operation == "daemon.topbar.context_window_source" {
			sources = append(sources, record.Context["source"])
		}
	}
	if len(sources) != 2 || sources[0] != windowSourceAssumed || sources[1] != windowSourceVendor {
		t.Errorf("sources = %v, want [assumed, vendor] once each", sources)
	}
}
